{-# LANGUAGE DoAndIfThenElse #-}
module Parser.CSV(parseCSV) where

import qualified Data.Text as Dtext
import Text.ParserCombinators.Parsec

import qualified Control.Exception as Exception
import Text.ARFF as ARFF

import qualified Data.ByteString as BS
import Data.ByteString.Char8(pack,unpack)

import Data.List(sort)

csvFile = endBy Parser.CSV.line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSVt :: String -> String -> Either ParseError [[String]]
parseCSVt input filename = parse csvFile ("Error in : "++filename) input

-- | ParseCSV takes two arguments : contents of input file and its name. It returns the parsed data in the same format as ARFF file by creating 
-- a pseudo header. The pseudo header is created by parsing the file and inferencing the type of attributes. The function also checks for malformed 
-- data by checking that no of elemnts in every row is same. The function also handles any missing attributes by returning Nothing 
-- for missing attributes. Any parse errors are displayed with line no. information to help user pinpoint the error quickly.

parseCSV:: String -> String -> IO (Either String (Header, [[Maybe AttributeValue]]) )

parseCSV input filename = 
    case (parseCSVt input filename) of 
        Right a -> case output of 
                        Right a -> do
                                      dt <- dataHeader
                                      return (Right dt)
                                   where
                                       header = getHeader (transpose a)
                                       dataHeader = getData a header
                                              
                        Left a -> return (Left a)
                   where  
                        output= checkIntegrity (removeTrailingSpaces $ removeEmptyLines a) filename
                                       
        Left  a -> return (Left (show a))


-----------------------------------------------------------------------------------------------------------
--Functions for finding out data type of every attribute and generating arff type metadata
-----------------------------------------------------------------------------------------------------------

getData:: [[String]] -> IO Header -> IO (Header, [[Maybe AttributeValue]])        
getData x header= 
    do 
        pureHeader<-header
        let
            fdata = toattributeValue x $ map (\x->dataType x) $ attributes pureHeader
        return (pureHeader,fdata)             
                                            
toattributeValue::[[String]] -> [AttributeType] -> [[Maybe AttributeValue]]
toattributeValue (x:xs) dataType = (zipWith toattrVal dataType x):(toattributeValue xs dataType)
toattributeValue [] dataType = []


toattrVal:: AttributeType -> String ->  Maybe AttributeValue
toattrVal dataType x =
    case dataType of 
         Numeric ->  if x=="?" then Nothing
                     else
                         Just (NumericValue ((read x)::Double))
                         
         Nominal _ -> if x=="?" then Nothing
                      else 
                         Just (NominalValue (pack x))
         

--getAttributes::[[String]]->[Attribute]


transpose:: [[String]]->[[String]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

getHeader::[[String]]->IO Header
getHeader x = do
                attrs <- getAttrList x 1 []
                return Header{ title = (pack "CSVDATA"), attributes = attrs}

getAttrList:: [[String]] -> Int -> [Attribute] -> IO [Attribute]

getAttrList (x:xs) attrIndex curList= 
    do 
        dtype <- getType x
        let 
            curAttr = (Attribute {name= (pack ("attribute"++show(attrIndex))) , dataType = dtype })
        
        (getAttrList xs (attrIndex +1) (curList++[curAttr]))
        
getAttrList [] attrIndex curList = do
                                return curList

                 
getType :: [String] -> IO AttributeType
getType x = do
                ftype <- examineFeature x 
                case ftype of 
                        Left a -> return (Nominal (sort (getNominalList x [])))
                        Right a -> return Numeric
                        

examineFeature :: [String] -> IO (Either Exception.SomeException [a1])
examineFeature (x:xs) = 
        do 
            if (x=="?") then 
               examineFeature xs
            else
                do
                    feature <- Exception.try(Exception.evaluate ((read x)::Double))
                    case feature of 
                        Left a -> return (Left a)     
                        Right a -> examineFeature xs
                    
examineFeature [] = return (Right []) 


getNominalList :: [String] -> [BS.ByteString] -> [BS.ByteString]                
getNominalList (x:xs) curlist = getNominalList xs (uniqueInsert curlist x)
getNominalList [] curList = curList

uniqueInsert:: [BS.ByteString] -> String -> [BS.ByteString]
uniqueInsert x y = if y=="?" then x
                   else
                        if ((ispresent x y) == True) then x
                        else
                            (x++ [pack y]) 
                   where                     
                        ispresent (x:xs) y = 
                            if ((unpack x)==y) then True
                            else ispresent xs y
                        ispresent [] y = False

--------------------------------------------------------------------------------------------------------
--Functions for parsing and verifying format of data
--------------------------------------------------------------------------------------------------------


removeEmptyLines :: [[String]] -> [[String]]                                            
removeEmptyLines (x:xs) = case (filter (not.null) x) of 
                                [] -> removeEmptyLines xs
                                otherwise -> (filter (not.null) x) : (removeEmptyLines xs)

removeEmptyLines [] = []                                
                                                          
checkIntegrity :: [[String]] -> String -> (Either String [[String]])
checkIntegrity [] filename = Right []
checkIntegrity (x:xs) filename = 
                        if expLen == foundLen then
                            Right (x:xs)
                        else
                            Left ("Error in file "++filename++"\nNo of features in row "++show(foundLen)++ " do not match no of features in row "++show(foundLen+1))
                        
                            where 
                                expLen = (length xs) +1
                                foundLen = (length $ malformedInput xs (length x)) + 1

malformedInput (x:xs) len = if (length x) == len then
                                x : (malformedInput xs (length x))
                            else
                                []

malformedInput [] len = []                                      

removeTrailingSpaces:: [[String]] ->[[String]]
removeTrailingSpaces (x:xs) = (map (\x-> Dtext.unpack $ Dtext.strip $ Dtext.pack x) x ): (removeTrailingSpaces xs)
removeTrailingSpaces [] =[]
