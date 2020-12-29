module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda


data Equations = Equations
  { x1 :: Float
  , y1 :: Float
  , z1 :: Float
  , r1 :: Float
  , x2 :: Float
  , y2 :: Float
  , z2 :: Float
  , r2 :: Float
  , x3 :: Float
  , y3 :: Float
  , z3 :: Float
  , r3 :: Float
  } deriving (Generic)
instance FromJSON Equations
instance ToJSON Equations

handler :: Equations -> Context () -> IO (Either String String)
handler equations context = return (compute equations)

compute equations 
  | determinant equations == 0 = (Left "The system has infinitely many solutions.")
  | otherwise = (Right (concat (printSolution equations)))
    

printSolution equations = zipWith (++) ["X=", ", Y=", ", Z="] (map show (findSolution equations))

findSolution equations = map (/(determinant equations)) [detx equations, dety equations, detz equations]
 

determinant equations = 
  det (x1 equations) (y1 equations) (z1 equations) 
  (x2 equations) (y2 equations) (z2 equations) 
  (x3 equations) (y3 equations) (z3 equations)

detx equations = det (r1 equations) (y1 equations) (z1 equations) 
  (r2 equations) (y2 equations) (z2 equations) 
  (r3 equations) (y3 equations) (z3 equations)

dety equations = det (x1 equations) (r1 equations) (z1 equations) 
  (x2 equations) (r2 equations) (z2 equations) 
  (x3 equations) (r3 equations) (z3 equations)

detz equations = det (x1 equations) (y1 equations) (r1 equations) 
  (x2 equations) (y2 equations) (r2 equations) 
  (x3 equations) (y3 equations) (r3 equations)

det a b c d e f g h i = a*(e*i-f*h) - b*(d*i-g*f) + c*(d*h-e*g)