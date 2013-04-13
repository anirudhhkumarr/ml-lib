import Text.ARFF as ARFF
import System.IO  
import Control.Monad

--ByteString
import Data.ByteString.Char8(pack,unpack)
import qualified Data.ByteString as BS

--Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as Text

--Data.Maybe
import Data.Maybe(catMaybes)

--Data.Function
import Data.Function

--Data.List
import Data.List

instance Ord AttributeValue where
    compare (NumericValue a) (NumericValue b) = compare a b
    compare (NominalValue a) (NominalValue b) = compare a b

instance Eq AttributeValue where
    (NumericValue a) == (NumericValue b) =  a == b
    (NominalValue a) == (NominalValue b) =  a == b
--Classifer
data AttributeInfo = NUMERIC (Double,Double)
                    |NOMINAL [(BS.ByteString,Double)]--[(Nominal,count)]
instance Show AttributeInfo where
    show (NUMERIC x) = show x
    show (NOMINAL x) = show x
    
trainData::[Attribute]->[BS.ByteString]->[[AttributeValue]]->[[AttributeInfo]]
trainData attributesInfo (first:rest) input =(classInfo : (trainData attributesInfo rest restClassData))
                                                where
                                                    intialInfo = intialize attributesInfo 						
                                                    (classInfo,restClassData) = trainClass intialInfo first input 0 				      
trainData _ _ [] = []

intialize::[Attribute]->[AttributeInfo]
intialize = map foo

foo::Attribute->AttributeInfo
foo a =case (dataType a) of Nominal xs -> NOMINAL $ intializeNominal xs
                            Numeric -> NUMERIC (0.0,0.0)

intializeNominal:: [BS.ByteString]->[(BS.ByteString,Double)]
intializeNominal = map (\ x->(x,0))

trainClass::[AttributeInfo]->BS.ByteString->[[AttributeValue]]->Double->([AttributeInfo],[[AttributeValue]])
trainClass info first (x:xs) n 
    |(last x) == (NominalValue first) = trainClass (updateClassInfo info $ init x) first xs (n+1)
    | otherwise  = (finalizeInfo info n,x:xs)
trainClass info _ [] n = (finalizeInfo info n,[])

updateClassInfo::[AttributeInfo]->[AttributeValue]->[AttributeInfo]
updateClassInfo info object =  zipWith bar info object

bar::AttributeInfo->AttributeValue->AttributeInfo
bar (NUMERIC (x,y)) (NumericValue a) = NUMERIC (x+a,y+a^2)
bar (NOMINAL xs) (NominalValue a) = NOMINAL $ updateNominal xs a

updateNominal::[(BS.ByteString,Double)]->BS.ByteString->[(BS.ByteString,Double)]
updateNominal ((x1,x2):xs) bs
    | x1 == bs = (x1,x2+1):xs
    | otherwise = (x1,x2):updateNominal xs bs
updateNominal [] _ = []

parseARFF input = parse arff input    

sortByClass xs n = sortBy (compare `on` (!!(n-1))) xs  

main = do
        handle <- openFile "arff.arff" ReadMode
        contents <- hGetContents handle
        let input = case (parseARFF $ pack contents) of Partial k -> (k BS.empty)
                                                        x-> x
            (_header,_data) = case input of Done _ y ->y
            inputData = map catMaybes _data	
            numAttributes = length $ attributes _header
            Nominal classes = dataType $ last $ attributes _header
            attributesInfo = attributes _header	
            sortedInput = sortByClass inputData numAttributes	    
            print sortedInput	
        return ()
