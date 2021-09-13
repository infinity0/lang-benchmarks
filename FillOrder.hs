-- only needed for the dynamic types stuff
{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications #-}

import           Control.DeepSeq                  (NFData,  deepseq)
import           Control.Exception                (assert)
import           Control.Monad.ST                 (runST)
import           Control.Monad.Trans.State.Strict (StateT, evalState, evalStateT, runState, runStateT, state)
import           Control.Monad.Primitive          (PrimMonad, PrimState)
-- State.Lazy is about 60%-70% slower, generally informally deprecated by community
import           Criterion.Main                   (defaultMain, bgroup, bench, nf)
import qualified Data.Strict.Sequence             (fromList)
import qualified Data.Strict.Vector               (fromList)
import qualified Data.Sequence                    (fromList)
import qualified Data.Vector                      (fromList, toList)
import qualified Data.Vector.Storable             (fromList)
import qualified Data.Vector.Unboxed              (fromList)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Fusion.Bundle as VFB
import           System.IO                        (hGetContents, stdin)

-- Fill an order, doing 1 pass over the book.
-- Returns the remaining book, and the remaining order quantity.
-- Probably the solution that most experienced Haskellers would write.
fillOrder1 :: (Traversable t, Ord i, Num i) => t i -> i -> (t i, i)
fillOrder1 = runState . traverse (\c -> state $ \r -> let x = min c r in (c - x, r - x))

-- Haskell equivalent of Patrick's 2-pass solution in K: -': Order &+\ Book
-- Behaviour is different from fillOrder1:
-- * Returns filled quantity rather than remaining quantity
-- * Does not return leftovers for a partially-filled order.
-- * Specialised; only works for lists.
fillOrder2FilledOnlyS :: (Ord i, Num i) => [i] -> i -> [i]
fillOrder2FilledOnlyS book order =
  let xs = scanl ((min order .) . (+)) 0 book in zipWith (flip (-)) xs (tail xs)

-- fillOrder2FilledOnlyS generalised to any Traversable
-- For lists, behaviour is identical to fillOrder2FilledOnlyS.
fillOrder2FilledOnly :: (Traversable t, Ord i, Num i) => t i -> i -> t i
fillOrder2FilledOnly book order =
  let pass1 = evalState (traverse (\c -> state $ \prev -> let x = min order (c + prev) in (x, x)) book) 0 in
  evalState (traverse (\cur -> state $ \prev -> (cur - prev, cur)) pass1) 0

-- fillOrderFilled as a 1-pass traverse. Faster than the 2-pass version.
-- Behaviour is identical to fillOrder2FilledOnly.
-- Note the similarity to fillOrder1; however it performs better as it does not
-- have to calculate (c - x) for the remaining book.
fillOrder1FilledOnly :: (Traversable t, Ord i, Num i) => t i -> i -> t i
fillOrder1FilledOnly = evalState . traverse (\c -> state $ \r -> let x = min c r in (x, r - x))

-- An attempt at implementing fillOrder1 as a 2-pass scan, for a fair
-- comparison against it.
--
-- NOTE HOWEVER, behaviour is different from fillOrder1:
-- * Returns filled quantity rather than remaining quantity
-- i.e. behaviour is more similar to fillOrder2FilledOnly
--
-- It's not possible to fix this within the constraints of doing a 2-pass scan,
-- since the 1st pass destroys information about the original quantities. We
-- would have to do a 3rd pass to recover this information against the original.
--
-- OTOH, it's easy to calculate the leftovers for a partially-filled order,
-- similar to fillOrder1 and different from fillOrder2FilledOnly.
--
-- Anyway, it's slower than fillOrder1.
fillOrder2Filled :: (Traversable t, Ord i, Num i) => t i -> i -> (t i, i)
fillOrder2Filled book order =
  let pass1 = evalState (traverse (\c -> state $ \prev -> let x = min order (c + prev) in (x, x)) book) 0 in
  let (pass2, (_, r)) = runState (traverse (\cur -> state $ \(prev, _) -> (cur - prev, (cur, order - cur))) pass1) (0, 0) in
  (pass2, r)

-- below, we try to squeeze performance out of fillOrder1
-- using Vector.Unboxed and Vector.Storable

fillOrderPart :: (Monad m, Ord a, Num a) => a -> StateT a m a
fillOrderPart c = state $ \r -> let x = min c r in (c - x, r - x)

-- sadly the specialised vectors don't implement Traversable but instead
-- provide a technically-different but same-in-spirit utility called mapM
fillOrder1V :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder1V = runState . VG.mapM fillOrderPart

-- should be part of vector; https://github.com/haskell/vector/pull/417
forI_VGM :: (PrimMonad m, VGM.MVector v a) => v (PrimState m) a -> (Int -> m a) -> m ()
{-# INLINE forI_VGM #-}
forI_VGM v f = loop 0
  where
    loop i | i >= n    = return ()
           | otherwise = f i >>= VGM.unsafeWrite v i >> loop (i + 1)
    n = VGM.length v

-- should be part of vector; https://github.com/haskell/vector/pull/417
mapM_VGM :: (PrimMonad m, VGM.MVector v a) => (a -> m a) -> v (PrimState m) a -> m ()
{-# INLINE mapM_VGM #-}
mapM_VGM f v = forI_VGM v $ \i -> f =<< VGM.unsafeRead v i

-- manual mutations, slightly faster
fillOrder1VM :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder1VM book order = runST $ flip runStateT order $ do
  book' <- VG.thaw book
  mapM_VGM fillOrderPart book'
  VG.unsafeFreeze book'

-- performs horribly; see https://github.com/haskell/vector/issues/416
fillOrder1VFNaive :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder1VFNaive book order =
  runState (VG.unstreamM $ VFB.mapM fillOrderPart (VG.stream book)) order

-- bug in vector; defined but not exported. we re-defined it for our use here
-- see https://github.com/haskell/vector/issues/416
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => VFB.MBundle m u a -> m (v a)
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- analogue of Traversable.traverse with a (PrimMonad m) instead of (Applicative m) constraint
primTraverse :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> m b) -> v a -> m (v b)
{-# INLINE [1] primTraverse #-}
primTraverse f v = unstreamPrimM $ VFB.mapM f $ VG.stream v

-- Rust-level performance due to clever Haskell optimisations
-- Purely functional style; no manual mutations!
-- Not as clean as fillOrder1, but very close!
-- Most optimised version of fillOrder1
fillOrder1VF :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder1VF book order = runST $ runStateT (primTraverse fillOrderPart book) order

-- Most optimised version of fillOrder2FilledOnly
fillOrder2FilledOnlyVF :: (VG.Vector v i, Ord i, Num i) => v i -> i -> v i
fillOrder2FilledOnlyVF book order = runST $ do
  pass1 <- evalStateT (primTraverse (\c -> state $ \prev -> let x = min order (c + prev) in (x, x)) book) 0
  evalStateT (primTraverse (\cur -> state $ \prev -> (cur - prev, cur)) pass1) 0

-- Most optimised version of fillOrder2Filled
fillOrder2FilledVF :: (VG.Vector v i, Ord i, Num i) => v i -> i -> (v i, i)
fillOrder2FilledVF book order = runST $ do
  pass1 <- evalStateT (primTraverse (\c -> state $ \prev -> let x = min order (c + prev) in (x, x)) book) 0
  (pass2, (_, r)) <- runStateT (primTraverse (\cur -> state $ \(prev, _) -> (cur - prev, (cur, order - cur))) pass1) (0, 0)
  pure (pass2, r)

-- SomeX are existential type wrappers around type X; this is how Haskell does
-- type-safe yet dynamic (runtime) types

data SomeTraversable a where
  SomeTraversable :: forall t a. (NFData (t a), Traversable t) => !(t a) -> SomeTraversable a

data SomeFillOrderX where
  SomeFillOrder :: !(forall t i. (Traversable t, Ord i, Num i) => t i -> i -> (t i, i)) -> SomeFillOrderX
  SomeFillOrderOnly :: !(forall t i. (Traversable t, Ord i, Num i) => t i -> i -> t i) -> SomeFillOrderX

runChecks :: a -> a
runChecks x =
  assert (checkFillOrder1 id fillOrder1) $
  assert (checkFillOrder2Filled id fillOrder2Filled) $
  assert (checkFillOrder2FilledOnly id fillOrder1FilledOnly) $
  assert (checkFillOrder2FilledOnly id fillOrder2FilledOnly) $
  assert (checkFillOrder2FilledOnly id fillOrder2FilledOnlyS) $
  assert (checkFillOrder1 Data.Vector.Storable.fromList fillOrder1V) $
  assert (checkFillOrder1 Data.Vector.Storable.fromList fillOrder1VM) $
  assert (checkFillOrder1 Data.Vector.Storable.fromList fillOrder1VFNaive) $
  assert (checkFillOrder1 Data.Vector.Storable.fromList fillOrder1VF) $
  assert (checkFillOrder2Filled Data.Vector.Storable.fromList fillOrder2FilledVF) $
  assert (checkFillOrder2FilledOnly Data.Vector.Storable.fromList fillOrder2FilledOnlyVF) $
  x
 where
  checkFillOrder1 :: Eq (v Int) => ([Int] -> v Int) -> (v Int -> Int -> (v Int, Int)) -> Bool
  checkFillOrder1 g f =
    f (g ib1) io1 == (g ob11, oo11) &&
    f (g ib1) io2 == (g ob12, oo12)
  checkFillOrder2Filled :: Eq (v Int) => ([Int] -> v Int) -> (v Int -> Int -> (v Int, Int)) -> Bool
  checkFillOrder2Filled g f =
    f (g ib1) io1 == (g obr11, oo11) &&
    f (g ib1) io2 == (g obr12, oo12)
  checkFillOrder2FilledOnly :: Eq (v Int) => ([Int] -> v Int) -> (v Int -> Int -> v Int) -> Bool
  checkFillOrder2FilledOnly g f =
    f (g ib1) io1 == g obr11 &&
    f (g ib1) io2 == g obr12
  ib1 = [50,5,0,100,400]
  io1 = 135 :: Int
  io2 = 700
  ob11 = [0,0,0,20,400]
  obr11 = [50,5,0,80,0]
  oo11 = 0
  ob12 = [0,0,0,0,0]
  obr12 = [50,5,0,100,400]
  oo12 = 145

runBench :: Int -> [Int] -> IO ()
runBench order bookList0 =
  -- ensure input data is fully-evaluated before running benchmark
  bookList `deepseq`
  bookSeq `deepseq`
  bookVec `deepseq`
  bookVecU `deepseq`
  bookVecS `deepseq`
  bookSSeq `deepseq`
  bookSVec `deepseq`
  defaultMain $
    [ bgroup "static" staticBenches
    , bgroup "dynamic" dynamicBenches
    ]
  where
    bookSeq = Data.Sequence.fromList bookList0
    bookVec = Data.Vector.fromList bookList0
    bookVecU = Data.Vector.Unboxed.fromList bookList0
    bookVecS = Data.Vector.Storable.fromList bookList0
    bookSSeq = Data.Strict.Sequence.fromList bookList0
    bookSVec = Data.Strict.Vector.fromList bookList0
    bookList = Data.Vector.toList bookVec -- just to make sure list has no advantage for being the first to be defined
    -- as per criterion, `nf` fully-evaluates outputs as part of benchmark
    staticBenches =
      [ bgroup "list" $
        [ bench "fillOrder1" $ nf (fillOrder1 bookList) order
        , bench "fillOrder2Filled" $ nf (fillOrder2Filled bookList) order
        , bench "fillOrder1FilledOnly" $ nf (fillOrder1FilledOnly bookList) order
        , bench "fillOrder2FilledOnlyS" $ nf (fillOrder2FilledOnlyS bookList) order
        , bench "fillOrder2FilledOnly" $ nf (fillOrder2FilledOnly bookList) order
        ]
      , bgroup "strict-sequence" $
        [ bench "fillOrder1" $ nf (fillOrder1 bookSSeq) order
        , bench "fillOrder2Filled" $ nf (fillOrder2Filled bookSSeq) order
        ]
      , bgroup "strict-vector" $
        [ bench "fillOrder1" $ nf (fillOrder1 bookSVec) order
        , bench "fillOrder2Filled" $ nf (fillOrder2Filled bookSVec) order
        ]
      , bgroup "sequence" $
        [ bench "fillOrder1" $ nf (fillOrder1 bookSeq) order
        , bench "fillOrder2Filled" $ nf (fillOrder2Filled bookSeq) order
        ]
      , bgroup "vector" $
        [ bench "fillOrder1" $ nf (fillOrder1 bookVec) order
        , bench "fillOrder2Filled" $ nf (fillOrder2Filled bookVec) order
        ]
      , bgroup "vector-unboxed" $
        [ bench "fillOrder1V" $ nf (fillOrder1V bookVecU) order
        , bench "fillOrder1VM" $ nf (fillOrder1VM bookVecU) order
        , bench "fillOrder1VFNaive" $ nf (fillOrder1VFNaive bookVecU) order
        , bench "fillOrder1VF" $ nf (fillOrder1VF bookVecU) order
        , bench "fillOrder2FilledOnlyVF" $ nf (fillOrder2FilledOnlyVF bookVecU) order
        , bench "fillOrder2FilledVF" $ nf (fillOrder2FilledVF bookVecU) order
        ]
      , bgroup "vector-storable" $
        [ bench "fillOrder1V" $ nf (fillOrder1V bookVecS) order
        , bench "fillOrder1VM" $ nf (fillOrder1VM bookVecS) order
        , bench "fillOrder1VFNaive" $ nf (fillOrder1VFNaive bookVecS) order
        , bench "fillOrder1VF" $ nf (fillOrder1VF bookVecS) order
        , bench "fillOrder2FilledOnlyVF" $ nf (fillOrder2FilledOnlyVF bookVecS) order
        , bench "fillOrder2FilledVF" $ nf (fillOrder2FilledVF bookVecS) order
        ]
      ]
    -- dynamic form below is unfortunately ~2x slower because it destroys the
    -- ability of GHC to specialise fillOrder1 etc at call sites. It needs to:
    -- 1. unroll the internal loop of fmap in dynamicBenches at compile time
    -- 2. specialise across a GADT existential-type boundary
    -- Both are hard; understandably GHC does neither yet, as of version 8.10
    books :: [(String, SomeTraversable Int)] =
      [ ("list", SomeTraversable bookList)
      , ("strict-sequence", SomeTraversable bookSSeq)
      , ("strict-vector", SomeTraversable bookSVec)
      , ("sequence", SomeTraversable bookSeq)
      , ("vector", SomeTraversable bookVec)
      ]
    funcs :: [(String, SomeFillOrderX)] =
      [ ("fillOrder1", SomeFillOrder fillOrder1)
      , ("fillOrder2Filled", SomeFillOrder fillOrder2Filled)
      , ("fillOrder1FilledOnly", SomeFillOrderOnly fillOrder1FilledOnly)
      , ("fillOrder2FilledOnly", SomeFillOrderOnly fillOrder2FilledOnly)
      ]
    dynamicBenches =
      (\(gname, SomeTraversable bt) -> bgroup gname $
        (\(bname, bf') -> case bf' of
          SomeFillOrder bf -> bench bname $ nf (bf bt) order
          SomeFillOrderOnly bf -> bench bname $ nf (bf bt) order
        ) <$> funcs
      ) <$> books

main :: IO ()
main = do
  order:book  <- fmap read . lines <$> hGetContents stdin
  putStrLn $ "order: " <> show order
  putStrLn $ "book length: " <> show (length book)
  runChecks $ runBench order book
