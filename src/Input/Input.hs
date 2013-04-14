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

--Data.Map
import qualified Data.Map as Map

instance Ord AttributeValue where
    compare (NumericValue a) (NumericValue b) = compare a b
    compare (NominalValue a) (NominalValue b) = compare a b

instance Eq AttributeValue where
    (NumericValue a) == (NumericValue b) =  a == b
    (NominalValue a) == (NominalValue b) =  a == b

--Classifer
data AttributeInfo = NUMERIC (Double,Double)
                    |NOMINAL (Map.Map BS.ByteString Double)

instance Show AttributeInfo where
    show (NUMERIC x) = show x
    show (NOMINAL x) = show x

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

trainData::[Attribute]->[BS.ByteString]->[[AttributeValue]]->[[AttributeInfo]]
trainData attributesInfo (first:rest) input = (classInfo : (trainData attributesInfo rest restClassData))
                                              where
                                                intialInfo = intialize attributesInfo
                                                (classInfo,restClassData) = trainClass intialInfo first input 0     			      
trainData _ _ [] = []

intialize::[Attribute]->[AttributeInfo]
intialize = map foo
            where
                --foo::Attribute->AttributeInfo
                foo a =case (dataType a) of Nominal xs -> NOMINAL $ intializeNominal xs
                                            Numeric -> NUMERIC (0.0,0.0)

intializeNominal:: [BS.ByteString]->(Map.Map BS.ByteString Double)
intializeNominal xs = Map.fromList $ map (\ x->(x,0)) xs

trainClass::[AttributeInfo]->BS.ByteString->[[AttributeValue]]->Double->([AttributeInfo],[[AttributeValue]])
trainClass info first (x:xs) n 
    |(last x) == (NominalValue first) = trainClass (updateClassInfo info $ init x) first xs (n+1)
    | otherwise  = (finalizeInfo info n,x:xs)
trainClass info _ [] n = (finalizeInfo info n,[])

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

test::(Header,[(BS.ByteString,[AttributeInfo])])->FilePath->IO ([BS.ByteString])
test (trainHeader,classifier) testFilePath = do 
                                                testHandle <- openFile testFilePath ReadMode
                                                testContents <- hGetContents testHandle
                                                let 
                                                    testInput = case (parseARFF $ pack testContents) of 
                                                                Partial k -> (k BS.empty)
                                                                x-> x
                                                    (testHeader,testdata) = case testInput of Done _ y ->y
                                                    --Drop data objects with one or more than missing feature value
                                                    inputData = map catMaybes testdata	
                                                    Nominal classes = dataType $ last $ attributes testHeader
                                                return $ testData classifier $ map (init) inputData

testData::[(BS.ByteString,[AttributeInfo])]->[[AttributeValue]]->[BS.ByteString]
testData classifier inputData = map (testObject classifier) inputData 

testObject ::[(BS.ByteString,[AttributeInfo])] -> [AttributeValue] -> BS.ByteString
testObject classifier object =fst $ last $ sortBy (compare `on` (snd)) $ map (foo object) classifier
                                where 
                                    foo object (classs,values) = (classs,computeProbability object values)

computeProbability ((NominalValue a):xs) ((NOMINAL y):ys) = (lookup' a y) * (computeProbability xs ys)    
computeProbability ((NumericValue a):xs) ((NUMERIC (mu,sigma)):ys) = 1/sqrt(2*pi*sigma^2)*exp(-(a-mu)^2/(2*sigma^2))*(computeProbability xs ys)	
computeProbability [] [] = 1.0

lookup' a y = case Map.lookup a y of
                Just x->x

parseARFF input = parse arff input    

sortByClass xs n = sortBy (compare `on` (!!(n-1))) xs  

getdifference::[AttributeValue]->[BS.ByteString]->Int
getdifference ((NominalValue x):xs) (y:ys) = if x == y then getdifference xs ys else 1+getdifference xs ys
getdifference [] [] = 0

classify x y = do
                (header,classifier)<- train x
                --print classifier
                (originClasses,learnedClasses) <- test (header,classifier) y
                print $ getdifference originClasses learnedClasses
                return ()

