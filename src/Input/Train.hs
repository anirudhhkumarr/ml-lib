module Train (train,AttributeInfo(..)) where

import Input(parseARFF)
import Text.ARFF as ARFF
import System.IO  
import Control.Monad

--Data.ByteString
import Data.ByteString.Char8(pack,unpack)
import qualified Data.ByteString as BS

--Data.Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as Text

--Data.Maybe
import Data.Maybe(catMaybes)

--Data.Map
import qualified Data.Map as Map

--Data.List
import Data.List

--Data.Function
import Data.Function

--Classifer
data AttributeInfo = NUMERIC (Double,Double) -- For calculation of mu and sigma 
                    |NOMINAL (Map.Map BS.ByteString Double)--for calculation of probability 
instance Show AttributeInfo where
    show (NUMERIC x) = show x
    show (NOMINAL x) = show x

------------------------------------------------------------------------------------------------------------
-- Ordering and Equivalence instance for data type AttributeValue
instance Ord AttributeValue where
    compare (NumericValue a) (NumericValue b) = compare a b
    compare (NominalValue a) (NominalValue b) = compare a b

instance Eq AttributeValue where
    (NumericValue a) == (NumericValue b) =  a == b
    (NominalValue a) == (NominalValue b) =  a == b
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
-- Equivalence instance for data type AttributeValue
instance Eq Header where
    a == b = (title a == title b) && (attributes a == attributes b)

instance Eq Attribute where
    a == b = (dataType a == dataType b) && (name a == name b)

instance Eq AttributeType where
    Numeric == a = case a of Numeric -> True
                             Nominal _ -> False
    Nominal xs == a = case a of Numeric -> False
                                Nominal ys -> xs == ys 
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
train::FilePath->IO (Either String (Header,[(BS.ByteString,[AttributeInfo])]))
train testFilePath = 
    do
        trainHandle <- openFile testFilePath ReadMode
        trainContents <- hGetContents trainHandle
        let
            trainInput = 
                case resultExpr of 
                    Done _ k -> Right k
                    Fail remInput contexts msg ->
                        Left ("In Train Data :\nRemaining input is :\n" ++ 
                        (unpack remInput) ++ "Invalid row supplied :\n"++ 
                        foldl (++) "" contexts)
                    
                where
                resultExpr = 
                    case (parseARFF $ pack trainContents) of
                        Partial k -> (k BS.empty)
                        k -> k

                
            returnValue = 
                case trainInput of
                    Right a ->  
                        Right (trainHeader,Map.toList $ trainData attributesInfo classes inputData)                               
                        where
                            (trainHeader,traindata) = a
                            --Drop data objects with one or more than missing feature value
                            inputData = filter (not . null) $ map removeNothing traindata
                            numAttributes = length $ attributes trainHeader
                            Nominal classes = dataType $ last $ attributes trainHeader
                            attributesInfo = attributes trainHeader
                    Left a -> Left a           
        
        return returnValue
----------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------
--Function for learning various parameters such as {mu(mean),sigma(standard deviation)} and probability for numeric and nominal features for various --classes 
trainData::[Attribute]->[BS.ByteString]->[[AttributeValue]]->(Map.Map BS.ByteString [AttributeInfo])
trainData attributesInfo classes input = trainClasses intialClassInfo input
                                         where
                                            intialInfo = intialize $ init attributesInfo
                                            intialClassInfo = Map.fromList $ map (\ a -> (a,(intialInfo,0))) classes
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------

removeNothing::[Maybe a] ->[a]
removeNothing xs = if length ys == length xs
                      then ys
                      else []
                   where    
                       ys = catMaybes xs

--Intialize feature values for a class
--1. For numeric feature intialize with (0.0,0.0)
--2.For nominal feature instialize with 0.0. 
intialize::[Attribute]->[AttributeInfo]
intialize = map foo
            where
                foo a = case (dataType a) of 
                        Nominal xs -> NOMINAL $ intializeNominal xs
                        Numeric -> NUMERIC (0.0,0.0)        
        --foo::Attribute->AttributeInfo
        -- foo intialize values for Nominal and Numeric features accordingly.

intializeNominal:: [BS.ByteString]-> (Map.Map BS.ByteString Double)
intializeNominal xs = Map.fromList $ map (\ x->(x,0)) xs
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
--trainClass learns parameters for a given class and retruns rest of input and values learned.
trainClasses::(Map.Map BS.ByteString ([AttributeInfo],Double))->[[AttributeValue]]->(Map.Map BS.ByteString [AttributeInfo])
trainClasses info (x:xs) = trainClasses updatedInfo xs 
                           where
                            NominalValue objectClass = last x
                            f (classInfo,count)= (updateClassInfo classInfo x,count+1)
                            updatedInfo = Map.adjust (\x-> f x) objectClass info 
trainClasses info [] = Map.map (\x->finalizeInfo x) info 

--updateClassInfo updates values of parameters as it sees a new data object
updateClassInfo::[AttributeInfo]->[AttributeValue]->[AttributeInfo]
updateClassInfo info object = zipWith bar info object
                              where
                                bar (NUMERIC (x,y)) (NumericValue a) = NUMERIC (x+a,y+a^2)
                                bar (NOMINAL xs) (NominalValue a) = NOMINAL $ Map.adjust (1+) a xs
                                --bar::AttributeInfo->AttributeValue->AttributeInfo

finalizeInfo::([AttributeInfo],Double)->[AttributeInfo]
finalizeInfo ((NOMINAL x):xs,n) = (NOMINAL $ Map.map (/n) x):finalizeInfo (xs,n)
finalizeInfo ((NUMERIC (x1,x2):xs),n) = (NUMERIC (mu,sigma)):finalizeInfo (xs,n)
                                        where
                                            mu = x1/n
                                            sigma = sqrt(if ((x2/n)-mu^2) == 0 then 0.000000000001 else (x2/n)-mu^2)
finalizeInfo ([],_) = []

