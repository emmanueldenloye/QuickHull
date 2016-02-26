-- | An attempt at writing QuickHull in quintessential Haskell.
-- | I am just writing the simple 2D version for now.

module QuickHull (qhull2D) where

import           Control.Arrow
import           Control.Monad
import           Data.Foldable                (asum)
import qualified Data.List                    as L (splitAt)
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                     as S
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro as I
import           Data.Vector.Generic.Mutable  as GM
import qualified Data.Vector.Unboxed          as UV
import           Numeric.LinearAlgebra        (peps)

data Region2D
    = R021
    | R201
    | R012
    deriving (Show,Eq,Ord)

data HullType
    = Hull { hull :: UV.Vector Double} -- Represents a point that is part of the convex hull.
    | Test { test :: UV.Vector Double} -- Represents a point needs to be tested.
    deriving (Eq,Show,Ord)

qhull2D
    :: V.Vector (UV.Vector Double)
    -> Either (V.Vector (UV.Vector Double)) (S.Set (UV.Vector Double))
qhull2D vecs
  | V.length vecs < 4 = Left vecs
  | otherwise =
      Right . S.map hull $
      qhull2DHelper (Hull ax) (Hull by) regU021 `S.union`
      qhull2DHelper (Hull by) (Hull bx) regU201 `S.union`
      qhull2DHelper (Hull bx) (Hull ay) regB201 `S.union`
      qhull2DHelper (Hull ax) (Hull ay) regB021
  where
    ((regU021,regU201),(regB021,regB201)) =
        (assignTwoRegions *** assignTwoRegions) . splitValidQuad ax by bx ay $
        V.map unmark marked
    ([ax,bx,ay,by],marked) = qhull2DInit vecs
    unmark (Hull v) = v
    unmark (Test v) = v
    -- unmark _ = error "You shouldn't get here"

markPoints :: V.Vector (UV.Vector Double) -> V.Vector HullType
markPoints = V.map Test

qhull2DInit :: V.Vector (UV.Vector Double)
            -> ([UV.Vector Double], V.Vector HullType)
qhull2DInit points =
    ( [ V.unsafeIndex points ax
      , V.unsafeIndex points bx
      , V.unsafeIndex points ay
      , V.unsafeIndex points by]
    , marked)
  where
    (ax,bx) = minMaxXIndexes points
    (ay,by) = minMaxYIndexes points
    marked = setMinMax . markPoints $ points
    setMinMax =
        V.modify
            (\w ->
                  do GM.unsafeModify w toHull ax
                     GM.unsafeModify w toHull bx
                     GM.unsafeModify w toHull ay
                     GM.unsafeModify w toHull by)
      where
        toHull (Test x) = Hull x
        toHull x@_ = x

qhull2DHelper :: HullType -> HullType -> V.Vector HullType -> S.Set HullType
qhull2DHelper (Hull v) (Hull w) vecs
  | V.length vecs > 1 =
      S.fromList [Hull w0, Hull w1, Hull w2] `S.union`
      qhull2DHelper (Hull w0) (Hull w1) r021 `S.union`
      qhull2DHelper (Hull w0) (Hull w2) r012 `S.union`
      qhull2DHelper (Hull w1) (Hull w2) r201
  | V.length vecs == 1 =
      S.singleton .
      (\(Test t) ->
            Hull t) .
      V.unsafeHead $
      vecs
  | otherwise = S.empty
  where
    (r021,r201,r012) = assignThreeRegions $ splitValidTri w0 w1 w2 testpts
    [w0,w1,w2] = V.toList . sortTriangle v w $ maxP
    testpts' = getPreTestPts vecs
    testpts =
        V.ifilter
            (\i _ ->
                  i /= maxInd)
            testpts'
    (maxInd,maxP) = maxDistPt v w testpts'
qhull2DHelper _ _ _ = error "You shouldn't get here!"

getPreTestPts :: V.Vector HullType -> V.Vector (UV.Vector Double)
getPreTestPts = V.map test . V.filter pred'
  where
    pred' (Test _) = True
    pred' _ = False

assignTwoRegions :: V.Vector (Region2D, UV.Vector Double)
                 -> (V.Vector HullType, V.Vector HullType)
assignTwoRegions = (,) <$> rMap R021 <*> rMap R201 -- . sortOnFirst

assignThreeRegions
    :: V.Vector (Region2D, UV.Vector Double)
    -> (V.Vector HullType, V.Vector HullType, V.Vector HullType)
assignThreeRegions = (,,) <$> rMap R021 <*> rMap R201 <*> rMap R012 -- . sortOnFirst

rMap
    :: Eq b
    => b -> V.Vector (b, UV.Vector Double) -> V.Vector HullType
rMap r = V.map (Test . snd) . V.filter ((== r) . fst)

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
