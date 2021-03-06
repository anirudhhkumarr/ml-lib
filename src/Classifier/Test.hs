{-# LANGUAGE DoAndIfThenElse #-}
module Classifier.Test (test,getClasses) where 

import Parser.ARFF(parseARFF)
import Parser.CSV(parseCSV)
import Classifier.Train(AttributeInfo(..),parseLinebyLine,getContextString) 
import Text.ARFF as ARFF
import System.IO  
import Control.Monad

--split string
import Data.List.Split(splitOn)



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

-- | Data type for Missing Values which is used in filling missing values 
data MissingValue = NumValue Double -- ^ Value constructor corresponding to Numeric missing values
                   |NomValue (Map.Map BS.ByteString Double) -- ^ Value constructor corresponding to Nominal missing values 

-- | Show instance for datatype MissingValue
instance (Show MissingValue) where
    show (NomValue x) = show x
    show (NumValue x) = show x

-- | This function takes a classifier and testfile as input and returns predicted classes and orginal classes for each object in testfile.
test::(Header,[(BS.ByteString,[AttributeInfo])])->FilePath->IO (Either String (([AttributeValue],[AttributeValue])))
test (trainHeader,classifier) testFilePath = 
    do 
        testHandle <- openFile testFilePath ReadMode
        testContents <- hGetContents testHandle
        if (last $ splitOn "." testFilePath) == "arff" then
            let
                (x:xs) = lines testContents
                resultExpr = parseLinebyLine testFilePath (parseARFF $ pack (x++"\n")) xs x 1
            in
                case resultExpr of       
                    Right (Done _ k) -> if testHeader == trainHeader 
                                        then 
                                            return (Right (convertNothing $ map last testdata ,testData classifier completeData))
                                        else
                                            return (Left $ contextHeaderError trainHeader testHeader )
                                        where
                                            (testHeader,testdata) = k
                                            attributesInfo = init $ attributes testHeader
                                            intial = intializeMissingValues attributesInfo
                                            computeValues = computeMissingValues intial $ map init testdata
                                            avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                            completeData = fillMissingValues avgValues $ map init testdata                    
                    Left strerr -> return (Left strerr)

        else if (last $ splitOn "." testFilePath) == "csv" then
            do
                resultExpr <- parseCSV testContents testFilePath
                case resultExpr of       
                    Right k -> if testHeader == trainHeader 
                               then 
                                    return (Right (convertNothing $ map last testdata ,testData classifier completeData))
                                else
                                    return (Left $ contextHeaderError trainHeader testHeader )
                                where
                                    (testHeader,testdata) = k
                                    attributesInfo = init $ attributes testHeader
                                    intial = intializeMissingValues attributesInfo
                                    computeValues = computeMissingValues intial $ map init testdata
                                    avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                    completeData = fillMissingValues avgValues $ map init testdata                    
                    Left strerr -> return (Left strerr)
                    
        else
            return (Left ("The specified file format "++(last $ splitOn "." testFilePath) ++ "is not supported\n"))

getClasses::(Header,[(BS.ByteString,[AttributeInfo])])->FilePath->IO (Either String ([AttributeValue]))
getClasses (trainHeader,classifier) testFilePath = 
    do 
        testHandle <- openFile testFilePath ReadMode
        testContents <- hGetContents testHandle
        if (last $ splitOn "." testFilePath) == "arff" then
            let
                (x:xs) = lines testContents
                resultExpr = parseLinebyLine testFilePath (parseARFF $ pack (x++"\n")) xs x 1            
            in
                case resultExpr of       
                    Right (Done _ k) ->
                        if testHeader == trainHeader 
                        then 
                            let
                                attributesInfo = init $ attributes testHeader
                                intial = intializeMissingValues attributesInfo
                                computeValues = computeMissingValues intial $ map init testdata
                                avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                completeData = fillMissingValues avgValues $ map init testdata                    
                            in
                                return (Right (testData classifier completeData))

                        else if (title testHeader == title trainHeader) && (attributes testHeader == init (attributes trainHeader))
                        then
                            let
                                attributesInfo = attributes testHeader
                                intial = intializeMissingValues attributesInfo
                                computeValues = computeMissingValues intial testdata
                                avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                completeData = fillMissingValues avgValues testdata                    
                            in
                                return (Right (testData classifier completeData))
                        else
                            return (Left $ contextHeaderError trainHeader testHeader )
                        where 
                            (testHeader,testdata) = k

                    Left strerr -> return (Left strerr)

        else if (last $ splitOn "." testFilePath) == "csv" then
            do
                resultExpr <- parseCSV testContents testFilePath
                case resultExpr of       
                    Right k ->
                        if testHeader == trainHeader 
                        then 
                            let
                                attributesInfo = init $ attributes testHeader
                                intial = intializeMissingValues attributesInfo
                                computeValues = computeMissingValues intial $ map init testdata
                                avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                completeData = fillMissingValues avgValues $ map init testdata                    
                            in
                                return (Right (testData classifier completeData))

                        else if (title testHeader == title trainHeader) && (attributes testHeader == init (attributes trainHeader))
                        then
                            let
                                attributesInfo = attributes testHeader
                                intial = intializeMissingValues attributesInfo
                                computeValues = computeMissingValues intial testdata
                                avgValues = finalizeMissingValues computeValues (fromIntegral(length testdata))
                                completeData = fillMissingValues avgValues testdata                    
                            in
                                return (Right (testData classifier completeData))
                        else
                            return (Left $ contextHeaderError trainHeader testHeader )
                        where 
                            (testHeader,testdata) = k

                    Left strerr -> return (Left strerr)        
                    
        else
            return (Left ("The specified file format "++(last $ splitOn "." testFilePath) ++ "is not supported\n"))                    


-- | Converts Nothings to "?"        
convertNothing::[Maybe AttributeValue]->[AttributeValue]
convertNothing = map foo 
                    where
                        foo (Just x) = x
                        foo (Nothing) = NominalValue $ pack "?"
                        
contextHeaderError::Header->Header->String
contextHeaderError trainHeader testHeader = if title trainHeader /= title testHeader
                                            then "Relation name in testfile doen't match with relation name in train file" 
                                            else if (length $ attributes trainHeader) /= (length $ attributes testHeader)
                                            then "Number of attribute definitions in testfile doesn't match with trainfile"
                                            else check (attributes trainHeader) (attributes testHeader)
                                            where
                                                check (x:xs) (y:ys)  
                                                    |x == y = check xs ys
                                                    |otherwise = case (dataType y) of
                                                                    Numeric -> str ++ " numeric"
                                                                    Nominal xs -> str ++ " {" ++ (init $foo xs) ++"}"
                                                                    where
                                                                         str ="Definition of attribute "++ unpack (name x) ++
                                                                              " doesn't match with its definition in train file.\n" ++
                                                                              "Perhaps you meant these :\n\t\t@attribute "++ unpack (name x) 
                                                                         foo (x:xs) = unpack x ++ ","++foo xs
                                                                         foo [] = ""                                                          
                                                check [] [] = "Header match "
------------------------------------------------------------------------------------------------------------

fillMissingValues values input = map (foo values) input  
                    where 
                        foo (x:xs) (Nothing:ys) = x:foo xs ys
                        foo (x:xs) (Just y:ys) = y:foo xs ys
                        foo [] [] = []
                                 
computeMissingValues::[MissingValue]->[[Maybe AttributeValue]]->[MissingValue]
computeMissingValues intialValues xs = foldl foo intialValues xs
                                       where
                                        foo (NumValue z:zs) (Just (NumericValue y):ys) =(NumValue (z+y)):foo zs ys
                                        foo (NomValue z:zs) (Just (NominalValue y):ys) =(NomValue $ Map.adjust (+1) y z):foo zs ys
                                        foo (z:zs) (Nothing:ys) =z:foo zs ys
                                        foo [] [] = []                                     
                                                                                                                                                        
intializeMissingValues::[Attribute]->[MissingValue]
intializeMissingValues = map foo
            where
                foo a = case (dataType a) of 
                        Nominal xs -> NomValue $ intializeMissingNominal xs
                        Numeric -> NumValue 0.0        

intializeMissingNominal:: [BS.ByteString]-> (Map.Map BS.ByteString Double)
intializeMissingNominal xs = Map.fromList $ map (\ x->(x,0)) xs

finalizeMissingValues xs n = map foo xs 
                                 where
                                    foo (NomValue ys) = NominalValue $ fst $ last $ sortBy (compare `on` snd) $ Map.toList ys
                                    foo (NumValue y) = NumericValue (y/n)

------------------------------------------------------------------------------------------------------------                                                                   
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

