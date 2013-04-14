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
train::FilePath->IO (Header,[(BS.ByteString,[AttributeInfo])])
train testFilePath = do
                        trainHandle <- openFile testFilePath ReadMode
                        trainContents <- hGetContents trainHandle
                        let 
                            trainInput = case (parseARFF $ pack trainContents) of 
                                            Partial k -> (k BS.empty)
                                            x-> x
                            (trainHeader,traindata) = case trainInput of Done _ y ->y
                            --Drop data objects with one or more than missing feature value
                            inputData = map catMaybes traindata	
                            numAttributes = length $ attributes trainHeader
                            Nominal classes = dataType $ last $ attributes trainHeader
                            attributesInfo = attributes trainHeader	
                            sortedInput = sortByClass inputData numAttributes
                        return (trainHeader,zipWith (\ a b ->(a,b)) classes $ trainData attributesInfo (sort classes) sortedInput)	
----------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------
--Function for learning various parameters such as {mu(mean),sigma(standard deviation)} and probability for numeric and nominal features for various --classes 
trainData::[Attribute]->[BS.ByteString]->[[AttributeValue]]->[[AttributeInfo]]
trainData attributesInfo (first:rest) input = (classInfo : (trainData attributesInfo rest restClassData))
                                                where
                                                    intialInfo = intialize attributesInfo 						
                                                    (classInfo,restClassData) = trainClass intialInfo first input 0 				      
trainData _ _ [] = []
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
--Intialize feature values for a class
--1. For numeric feature intialize with (0.0,0.0)
--2.For nominal feature instialize with 0.0. 
intialize::[Attribute]->[AttributeInfo]
intialize = map foo
            where
                --foo::Attribute->AttributeInfo
                foo a =case (dataType a) of Nominal xs -> NOMINAL $ intializeNominal xs
                                            Numeric -> NUMERIC (0.0,0.0)

intializeNominal:: [BS.ByteString]-> (Map.Map BS.ByteString Double)
intializeNominal xs = Map.fromList $ map (\ x->(x,0)) xs
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
--trinClass learns parameters for a given class and retruns rest of input and values learned.
trainClass::[AttributeInfo]->BS.ByteString->[[AttributeValue]]->Double->([AttributeInfo],[[AttributeValue]])
trainClass info first (x:xs) n 
    |(last x) == (NominalValue first) = trainClass (updateClassInfo info $ init x) first xs (n+1)
    | otherwise  = (finalizeInfo info n,x:xs)
trainClass info _ [] n = (finalizeInfo info n,[])

--updateClassInfo updates values of parameters as it sees a new data object
updateClassInfo::[AttributeInfo]->[AttributeValue]->[AttributeInfo]
updateClassInfo info object =  zipWith bar info object
                                where
                                    --bar::AttributeInfo->AttributeValue->AttributeInfo
                                    bar (NUMERIC (x,y)) (NumericValue a) = NUMERIC (x+a,y+a^2)
                                    bar (NOMINAL xs) (NominalValue a) = NOMINAL $ Map.adjust (1+) a xs

finalizeInfo::[AttributeInfo]->Double->[AttributeInfo]
finalizeInfo ((NOMINAL x):xs) n = (NOMINAL $ Map.map (/n) x):finalizeInfo xs n
finalizeInfo ((NUMERIC (x1,x2)):xs) n = (NUMERIC (mu,sigma)):finalizeInfo xs n
                                        where
                                            mu = x1/n
                                            sigma = sqrt(if ((x2/n)-mu^2) == 0 then 0.000000000001 else (x2/n)-mu^2)
finalizeInfo [] _ = []
------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
--sortByClass function sorts input data according to class label
sortByClass xs n = sortBy (compare `on` (!!(n-1))) xs  
------------------------------------------------------------------------------------------------------------

