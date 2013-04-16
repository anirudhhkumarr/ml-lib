module NBClassifier(buildNBClassifier,testClassifier,predictClasses) where

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

------------------------------------------------------------------------------------------------------------
buildNBClassifier::FilePath->IO (Either String (Header,[(BS.ByteString,[AttributeInfo])]))
buildNBClassifier = train 

------------------------------------------------------------------------------------------------------------
testClassifier::(Either String (Header,[(BS.ByteString,[AttributeInfo])]))->FilePath->IO (Either String Double)
testClassifier trainOutput y =  
    do
        mtestOutput <- 
            case trainOutput of 
                Right a ->  test (header,classifier) y
                            where
                                (header,classifier) = a
                Left a -> return (Left a)
        case mtestOutput of 
            Right (originalClasses,inferencedClasses)->return (Right $ getAccuracy originalClasses inferencedClasses)
            Left x -> return (Left x)

getAccuracy::[AttributeValue]->[AttributeValue]->Double
getAccuracy originalClasses inferencedClasses= (v1-v2)*100/v1
                                                 where
                                                     v1 = fromIntegral(length originalClasses)
                                                     v2 = getdifference originalClasses inferencedClasses

getdifference (x:xs) (y:ys) 
    | NominalValue (pack "?") == x = getdifference xs ys
    | x == y = getdifference xs ys 
    | otherwise = 1+getdifference xs ys
getdifference [] [] = 0
------------------------------------------------------------------------------------------------------------

predictClasses::(Either String (Header,[(BS.ByteString,[AttributeInfo])]))->FilePath->IO (Either String ([AttributeValue]))
predictClasses trainOutput y =  
    do
        mtestOutput <- 
            case trainOutput of 
                Right a ->  getClasses (header,classifier) y
                            where
                                (header,classifier) = a
                Left a -> return (Left a)
        case mtestOutput of 
            Right classes->return (Right classes)
            Left x -> return (Left x)
------------------------------------------------------------------------------------------------------------

