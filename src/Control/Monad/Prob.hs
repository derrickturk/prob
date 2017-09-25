{-# LANGUAGE Rank2Types #-}

module Control.Monad.Prob (
    Prob
  , sampleProb
  , sampleProbRIO
  , always
  , binary
  , uniform
  , trials
  , meanMC
  , probMC
) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Lazy
import System.Random (random, randomR, Random, RandomGen, getStdRandom)

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

-- for now, probably over Foldable later
uniform :: [a] -> Prob a
uniform xs = Prob $ do
  let l = length xs
  i <- genR (0, l - 1)
  return $ xs !! i
