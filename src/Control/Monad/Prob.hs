{-# LANGUAGE Rank2Types #-}

module Control.Monad.Prob (
    Prob
  , sampleProb
  , sampleProbRIO
  , always
  , binary
  , choose
  , uniform
  , normal
  , normals
  , clamp
  , condition
  , trials
  , meanMC
  , probMC
  , shuffle
  , uniformDiscrete
  , withoutReplacement
) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Lazy
import System.Random (random, randomR, Random, RandomGen, getStdRandom)
import qualified System.Random.Shuffle as RS (shuffle)

newtype Prob a = Prob { sampleProb :: forall g . (RandomGen g) => State g a }

sampleProbRIO :: Prob a -> IO a
sampleProbRIO = getStdRandom . runState . sampleProb

instance Functor Prob where
  {-# INLINE fmap #-}
  fmap f p = Prob $ f <$> sampleProb p

instance Applicative Prob where
  pure = always
  af <*> ax = Prob $ do
    f <- sampleProb af
    x <- sampleProb ax
    return $ f x

instance Monad Prob where
  m >>= k = Prob $ do
    x <- sampleProb m
    sampleProb $ k x

gen :: (RandomGen g, Random a) => State g a
gen = do
  (x, s) <- random <$> get
  put s
  return x

genR :: (RandomGen g, Random a) => (a, a) -> State g a
genR (l, u) = do
  (x, s) <- randomR (l, u) <$> get
  put s
  return x

genQuantile :: (RandomGen g, Fractional p, Random p) => State g p
genQuantile = do
  (q, s) <- randomR (0, 1) <$> get
  put s
  return q

genStandardNormals :: (RandomGen g, Random p, Floating p, Ord p)
                      => State g (p, p)
genStandardNormals = do
  u1 <- genR (0, 1)
  u2 <- genR (0, 1)
  let r = sqrt $ (-2.0 * log u1)
      theta = 2.0 * pi * u2
      x1 = r * cos theta
      x2 = r * sin theta
  return (x1, x2)

clamp :: (Ord a) => (Maybe a, Maybe a) -> Prob a -> Prob a
clamp (Nothing, Nothing) = id
clamp (Just lb, Nothing) = fmap (max lb)
clamp (Nothing, Just ub) = fmap (min ub)
clamp (Just lb, Just ub) = fmap (max lb . min ub)

condition :: (a -> Bool) -> Prob a -> Prob a
condition p m = do
  x <- m
  if p x
    then return x
    else condition p m

trials :: Int -> Prob a -> Prob [a]
trials = replicateM

meanMC :: (Real a, Fractional b) => Int -> Prob a -> Prob b
meanMC n p = ((/ realToFrac n) . realToFrac . sum) <$> trials n p

probMC :: Fractional b => Int -> (a -> Bool) -> Prob a -> Prob b
probMC n e p = ((/ realToFrac n)
               . realToFrac
               . length
               . filter e
               ) <$> trials n p

-- we can't make these point-free e.g. Prob . return because fully polymorphic
--   types won't unify with our forall g rng type
always :: a -> Prob a
always e = Prob $ return e

binary :: (Random p, Fractional p, Ord p) => p -> a -> b -> Prob (Either a b)
binary p l r = Prob $ do
  q <- genQuantile
  if q <= p
    then return $ Left l
    else return $ Right r

-- this is a little redundant
choose :: (Random p, Fractional p, Ord p) => p -> Prob a -> Prob a -> Prob a
choose p l r = Prob $ do
  q <- genQuantile
  if q <= p then sampleProb l else sampleProb r

uniform :: (Random p) => (p, p) -> Prob p
uniform (l, u) = Prob $ genR (l, u)

normal :: (Random p, Floating p, Ord p) => p -> p -> Prob p
normal mean sd = Prob $ do
  (z, _) <- genStandardNormals
  return $ mean + z * sd

-- an optimized case
normals :: (Random p, Floating p, Ord p) => Int -> p -> p -> Prob [p]
normals n mean sd
  | n <= 0 = Prob $ return []
  | n == 1 = return <$> normal mean sd
  | n >= 2 = Prob $ do
      (z1, z2) <- genStandardNormals
      rest <- sampleProb $ normals (n - 2) mean sd
      return $ z1:z2:rest

shuffle :: [a] -> Prob [a]
shuffle xs = Prob $ do
  let l = length xs
  u <- traverse (\i -> genR (0, l - i)) [1..l]
  return $ RS.shuffle xs u

-- for now, probably over Foldable later
uniformDiscrete :: [a] -> Prob a
uniformDiscrete xs = Prob $ do
  let l = length xs
  i <- genR (0, l - 1)
  return $ xs !! i

withoutReplacement :: Int -> [a] -> Prob [a]
withoutReplacement n = (fmap $ take n) . shuffle
