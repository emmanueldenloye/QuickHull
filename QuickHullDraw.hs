-- | Now we plot!

module QuickHullDraw where

import           Control.Arrow
import           Data.List
import qualified Data.Set                as S
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as UV
import           Graphics.Rendering.Plot
import           Numeric.LinearAlgebra   (Vector (..))
import           QuickHull
import           System.Environment
import           System.Random

main :: IO ()
main = do
    [d] <- getArgs
    let vectors =
            V.fromList .
            map UV.fromList .
            chop 2 . take ((* 2) $ read d :: Int) . randomRs (0 :: Double, 100) $
            mkStdGen 0
        (rxL,rxH) = (0, 100)
        (ryL,ryH) = (0, 100)
        (vx,vy) = convertPoints vectors
        hull' = qhull2D vectors
        (hx,hy) =
            convertPoints . either id (V.fromList . S.toList) $ hull'
    writeFigure
        PNG
        "QHull.png"
        (600, 600)
        (testFig vx vy hx hy (rxL, rxH) (ryL, ryH))
    -- V.mapM_ print vectors
    -- putStrLn ""
    -- either
    --     (V.mapM_ print)
    --     (\v ->
    --           do mapM_ print v
    --              print $ S.size v) hull'

convertPoints :: V.Vector (UV.Vector Double) -> (Vector Double, Vector Double)
convertPoints =
    (V.convert *** V.convert) .
    V.unzip . V.map (UV.unsafeHead &&& UV.unsafeLast)

testFig
    :: Series
    -> Series
    -> Series
    -> Series
    -> (Double, Double)
    -> (Double, Double)
    -> Figure ()
testFig xs ys hx hy (rxL,rxH) (ryL,ryH) = do
    withTextDefaults $ setFontFamily "OpenSymbol"
    withTitle $ setText "2D Convex Hull with QuickHull!"
    setPlots 1 1
    withPlot (1, 1) $
        do setDataset [(xs, point ys (Asterisk, red)),(hx, point hy (Box, purple))]
           addAxis XAxis (Side Lower) $ withAxisLabel $ setText "x-axis"
           addAxis YAxis (Side Lower) $ withAxisLabel $ setText "y-axis"
           addAxis XAxis (Value 0) $ return ()
           setRange XAxis Lower Linear (rxL-5) (rxH+5)
           setRange YAxis Lower Linear (ryL-5) (ryH+5)

chop :: Int -> [a] -> [[a]]
chop n =
    unfoldr
        (\v ->
              if null v
                  then Nothing
                  else Just $ splitAt n v)
