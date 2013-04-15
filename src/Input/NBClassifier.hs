module NBClassifier where

import Text.ARFF as ARFF
import Test
import Train
import System.IO  
import Control.Monad

--Data.ByteString
import Data.ByteString.Char8(pack,unpack)
import qualified Data.ByteString as BS


--Data.Maybe
import Data.Maybe(catMaybes)

--Data.Map
import qualified Data.Map as Map

--Data.List
import Data.List

--Data.Function
import Data.Function


getdifference::[AttributeValue]->[AttributeValue]->Int
getdifference (x:xs) (y:ys) = if NominalValue (pack "?") == x 
                              then 
                                getdifference xs ys
                              else if x == y 
                              then
                                 getdifference xs ys 
                              else 
                                1+getdifference xs ys
getdifference [] [] = 0
classify x y = 
    do
        trainOutput <- train x
        let 
            mtestOutput = 
                case trainOutput of 
                    Right a ->  test (header,classifier) y
                                where
                                    (header,classifier) = a
                    Left a -> return (Left a)                                       
            
        --print classifier
        testOutput <- mtestOutput 
        case testOutput of 
            Right a -> do
                        putStrLn "Original Classes "
                        print originClasses
                        putStrLn "Inferenced Classes "
                        print learnedClasses
                        putStrLn "No of records  mismatched "
                        print $ (getdifference originClasses learnedClasses)
                        where
                            (originClasses,learnedClasses)=a
            Left a -> putStrLn a        
        return ()
