module Test (test) where 

import Input(parseARFF)
import Train(AttributeInfo(..)) 
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

----------------------------------------------------------------------------------------------------------------

test::(Header,[(BS.ByteString,[AttributeInfo])])->FilePath->IO (([AttributeValue],[BS.ByteString]))
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
                                                return (map last inputData ,testData classifier $ map init inputData)

testData::[(BS.ByteString,[AttributeInfo])]->[[AttributeValue]]->[BS.ByteString]
testData classifier inputData = map (testObject classifier) inputData 

testObject classifier object = fst $ last $ sortBy (compare `on` (snd)) $ map (foo object) classifier
                                where 
                                    foo object (classs,values) = (classs,computeProbability object values)

computeProbability ((NominalValue a):xs) ((NOMINAL y):ys) = (lookup' a y) * (computeProbability xs ys)	
computeProbability ((NumericValue a):xs) ((NUMERIC (mu,sigma)):ys) = 1/sqrt(2*pi*sigma^2)*exp(-(a-mu)^2/(2*sigma^2))*(computeProbability xs ys)	
computeProbability [] [] = 1.0

lookup' a y = case Map.lookup a y of
                Just x->x
------------------------------------------------------------------------------------------------------------

