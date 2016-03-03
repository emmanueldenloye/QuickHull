-- | An attempt at writing QuickHull in quintessential Haskell.
-- | I am just writing the simple 2D version for now.

module QuickHull (qhull2D) where

import           Control.Arrow
import           Control.Monad
import           Data.Foldable                (asum)
import qualified Data.List                    as L (splitAt)
import           Data.Maybe
import           Data.Ord
-- import qualified Data.Set                     as S
import           Data.DList                   as D
-- import           Data.Monoid
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro as I
-- import           Data.Vector.Generic.Mutable  as GM
import qualified Data.Vector.Unboxed          as UV
import           Numeric.LinearAlgebra        (peps)

data Region2D
    = R021
    | R201
    | R012
    deriving (Show,Eq,Ord)

qhull2D
    :: V.Vector (UV.Vector Double)
    -> Either (V.Vector (UV.Vector Double)) [UV.Vector Double]
qhull2D vecs
  | V.length vecs < 4 = Left vecs
  | otherwise =
      Right . D.toList $
        qhull2DHelper ax by regU021 `mappend`
        qhull2DHelper by bx regU201 `mappend`
        qhull2DHelper bx ay regB201 `mappend`
        qhull2DHelper ax ay regB021
  where
    ((regU021, regU201), (regB021, regB201)) =
      (assignTwoRegions *** assignTwoRegions) . splitValidQuad ax by bx ay $
        vecs
    [ax, bx, ay, by] = qhull2DInit vecs

qhull2DInit :: V.Vector (UV.Vector Double) -> [UV.Vector Double]
qhull2DInit points =
  [ V.unsafeIndex points ax
  , V.unsafeIndex points bx
  , V.unsafeIndex points ay
  , V.unsafeIndex points by
  ]
  where
    (ax, bx) = minMaxXIndexes points
    (ay, by) = minMaxYIndexes points

qhull2DHelper :: UV.Vector Double -> UV.Vector Double -> V.Vector (UV.Vector Double) -> D.DList (UV.Vector Double)
qhull2DHelper v w vecs
  | V.length vecs > 1 =
      D.cons maxP $
        qhull2DHelper w0 w1 r021 `mappend`
        qhull2DHelper w0 w2 r012 `mappend`
        qhull2DHelper w1 w2 r201
  | V.length vecs == 1 =
      D.singleton $
        V.unsafeHead vecs
  | otherwise = D.empty
  where
    (r021, r201, r012) = assignThreeRegions $ splitValidTri w0 w1 w2 testpts
    [w0, w1, w2] = V.toList . sortTriangle v w $ maxP
    testpts = V.ifilter (\i _ -> i /= maxInd) vecs
    (maxInd, maxP) = maxDistPt v w vecs

assignTwoRegions :: V.Vector (Region2D, UV.Vector Double)
                 -> (V.Vector (UV.Vector Double), V.Vector (UV.Vector Double))
assignTwoRegions = (,) <$> rMap R021 <*> rMap R201

assignThreeRegions
    :: V.Vector (Region2D, UV.Vector Double)
    -> (V.Vector (UV.Vector Double), V.Vector (UV.Vector Double), V.Vector (UV.Vector Double))
assignThreeRegions = (,,) <$> rMap R021 <*> rMap R201 <*> rMap R012

rMap
    :: Eq b
    => b -> V.Vector (b, UV.Vector Double) -> V.Vector (UV.Vector Double)
rMap r = V.map snd . V.filter ((== r) . fst)

-- Assumes points are 2D. Sorts points by their x-coordinates
sortTriangle
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> V.Vector (UV.Vector Double)
sortTriangle u v w = V.modify (I.sortBy (comparing UV.unsafeHead)) r
  where
    r = V.fromList [u, v, w]

inTriangleInterior
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> (Bool, Double, [Double])
inTriangleInterior a b c p = (,,) <$> same' . sum <*> signum . sum <*> id $ vv
  where
    vv = [test' ab aP, test' bc bp, test' ca cp]
    ab = diff b a
    bc = diff c b
    ca = diff a c
    aP = diff p a
    bp = diff p b
    cp = diff p c
    diff = UV.zipWith (-)
    same' = (||) <$> (== 3) <*> (== -3)
    test' u v = signum $ UV.foldl1' (-) $ UV.zipWith (*) u (UV.reverse v)

inQuadInterior
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> (Bool, Double, [Double])
inQuadInterior a b c d p = (,,) <$> same' . sum <*> signum . sum <*> id $ vv
  where
    vv = [test' ab aP, test' bc bp, test' cd cp, test' da dp]
    ab = diff b a
    bc = diff c b
    cd = diff d c
    da = diff a d
    aP = diff p a
    bp = diff p b
    cp = diff p c
    dp = diff p d
    diff = UV.zipWith (-)
    same' = (||) <$> (== 4) <*> (== (-4))
    test' u v = signum $ UV.foldl1' (-) $ UV.zipWith (*) u (UV.reverse v)

testInRegionQuad
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> (Maybe Region2D, Maybe Region2D)
testInRegionQuad v0 v1 v2 v3 p = reg
  where
    (inQuad,signSum,regs) = inQuadInterior v0 v1 v2 v3 p
    reg =
        if inQuad
            then (Nothing, Nothing)
            else (asum *** asum) . L.splitAt 2 . zipWith ($) regAssign $ regs
    regAssign =
        [ toMaybe R021 . (/= signSum)
        , toMaybe R201 . (/= signSum)
        , toMaybe R021 . (/= signSum)
        , toMaybe R201 . (/= signSum)]

splitValidQuad
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> V.Vector (UV.Vector Double)
    -> (V.Vector (Region2D, UV.Vector Double), V.Vector (Region2D, UV.Vector Double))
splitValidQuad u v w x =
    (V.map fromJust . V.filter isJust *** V.map fromJust . V.filter isJust) .
    V.unzip .
    V.map
        ((\v' (a,b) ->
               (flip (,) v' <$> a, flip (,) v' <$> b)) `ap`
         testInRegionQuad u v w x)

toMaybe :: a -> Bool -> Maybe a
toMaybe a pred' =
    if pred'
        then Just a
        else Nothing

testInRegionTri
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> Maybe Region2D
testInRegionTri v0 v1 v2 p = reg
  where
    (inTriangle,signSum,regs) = inTriangleInterior v0 v1 v2 p
    reg =
        if inTriangle
            then Nothing
            else asum . zipWith ($) regAssign $ regs
    regAssign =
        [ toMaybe R021 . (/= signSum)
        , toMaybe R201 . (/= signSum)
        , toMaybe R012 . (/= signSum)]

splitValidTri
    :: UV.Vector Double
    -> UV.Vector Double
    -> UV.Vector Double
    -> V.Vector (UV.Vector Double)
    -> V.Vector (Region2D, UV.Vector Double)
splitValidTri u v w =
    V.map fromJust .
    V.filter isJust . V.map ((fmap . flip (,)) `ap` testInRegionTri u v w)

minMaxXIndexes :: V.Vector (UV.Vector Double) -> (Int, Int)
minMaxXIndexes = minmax . sortOnFirst . vzip . V.map UV.unsafeHead
  where
    vzip v = V.zip v (V.enumFromN 0 $ V.length v)

minMaxYIndexes :: V.Vector (UV.Vector Double) -> (Int, Int)
minMaxYIndexes = minmax . sortOnFirst . vzip . V.map UV.unsafeLast
  where
    vzip v = V.zip v (V.enumFromN 0 $ V.length v)

minmax :: V.Vector (Double, Int) -> (Int, Int)
minmax = uncurry (,) . (snd . V.unsafeHead &&& snd . V.unsafeLast)

sortOnFirst
    :: Ord a
    => V.Vector (a, b) -> V.Vector (a, b)
sortOnFirst = V.modify (I.sortBy (comparing fst))

maxDistPt
    :: UV.Vector Double
    -> UV.Vector Double
    -> V.Vector (UV.Vector Double)
    -> (Int, UV.Vector Double)
maxDistPt mn mx =
    getMax . ap V.zip (V.map $ internalBisect mn mx . snd) . V.indexed
  where
    getMax = fst . V.maximumBy (comparing snd)

internalBisect :: UV.Vector Double
               -> UV.Vector Double
               -> UV.Vector Double
               -> Double
internalBisect u v w = 2 * sqrt (b * c * s * (b + c)) / (b + c + peps)
  where
    dist' x = sqrt . UV.sum . UV.map (join (*)) . UV.zipWith (-) x
    a = dist' u v
    b = dist' v w
    c = dist' u w
    s = a + b + c
