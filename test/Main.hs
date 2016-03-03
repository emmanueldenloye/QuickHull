import           Data.List
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import           QuickHull
import           System.Environment
import           System.Random

main :: IO ()
main = do
    let d = 10000 :: Int
      vectors =
            V.fromList .
            map UV.fromList .
            chop 2 . take ((* 2) $ d) . randomRs (-100 :: Double, 100) $
            mkStdGen 0
    let hull' = qhull2D vectors
    either (V.mapM_ print) (mapM_ print) hull'

chop :: Int -> [a] -> [[a]]
chop n =
    unfoldr
        (\v ->
              if null v
                  then Nothing
                  else Just $ splitAt n v)
