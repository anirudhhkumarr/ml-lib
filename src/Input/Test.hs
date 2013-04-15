module Test (test) where 

import Input(parseARFF)
import Train(AttributeInfo(..),getContextString,removeNothing) 
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

test:: (Header,[(BS.ByteString,[AttributeInfo])])->FilePath->IO (Either String (([AttributeValue],[AttributeValue])))
test (trainHeader,classifier) testFilePath = 
    do 
        testHandle <- openFile testFilePath ReadMode
        testContents <- hGetContents testHandle
        let
            returnValue = 
                case resultExpr of       
                    Right (Done _ k) -> if testHeader == trainHeader 
                                        then 
                                            Right (map last inputData ,testData classifier $ map init inputData)
                                        else
                                            Left "Headers of train and test file doesn't match"
                                        where
                                            (testHeader,testdata) = k
                                            --Drop data objects with one or more than missing feature value
                                            inputData = filter (not . null) $ map removeNothing testdata
                                            Nominal classes = dataType $ last $ attributes testHeader
                    
                    Left strerr -> Left strerr
    
                where 
                    (x:xs) = lines testContents
                    resultExpr = parseLinebyLine (parseARFF $ pack (x++"\n")) xs x 1

        return returnValue

        
parseLinebyLine:: Result (Header, [[Maybe AttributeValue]]) -> [String] -> String -> Int -> Either String (Result (Header, [[Maybe AttributeValue]]))

parseLinebyLine initval (x:xs) prevline lineno = 
    case initval of 
        Partial k-> parseLinebyLine (k  $ pack (x++"\n")) xs x (lineno+1)
        Fail remInput contexts msg ->
            Left ("In Test Data:\nInvalid row supplied at line no "++(show lineno)++"\n" ++ prevline++"\n"++ getContextString contexts)

        k -> Right k
                              
parseLinebyLine initval [] prevline lineno = 
    case initval of
        Partial k-> Right $ k BS.empty
        Fail remInput contexts msg -> 
            Left ("In Test Data:\nInvalid row supplied at line no "++(show lineno)++"\n" ++ prevline++"\n"++  getContextString contexts)
            
        k-> Right k        
        
                       
testData::[(BS.ByteString,[AttributeInfo])]->[[AttributeValue]]->[AttributeValue]
testData classifier inputData = map (testObject classifier) inputData 

testObject classifier object = NominalValue $ fst $ last $ sortBy (compare `on` (snd)) $ map (foo object) classifier
                               where 
                                foo object (classs,values) = (classs,computeProbability object values)

computeProbability ((NominalValue a):xs) ((NOMINAL y):ys) = (lookup' a y) * (computeProbability xs ys)  
computeProbability ((NumericValue a):xs) ((NUMERIC (mu,sigma)):ys) = 1/sqrt(2*pi*sigma^2)*exp(-(a-mu)^2/(2*sigma^2))*(computeProbability xs ys) 
computeProbability [] [] = 1.0

lookup' a y = case Map.lookup a y of
                Just x->x
------------------------------------------------------------------------------------------------------------

