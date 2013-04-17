
module CSV() where

import Text.ParserCombinators.Parsec
import Train(AttributeInfo(..))

import qualified Control.Exception as Exception
import Text.ARFF as ARFF

import qualified Data.ByteString as BS
import Data.ByteString.Char8(pack,unpack)

csvFile = endBy CSV.line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSVt :: String -> String -> Either ParseError [[String]]
parseCSVt input filename = parse csvFile ("Error in : "++filename) input

parseCSV:: String -> String -> Either String [[AttributeValue]]

parseCSV input filename = 
    case (parseCSVt input filename) of 
        Right a -> case output of 
                        Right a -> Right (toattributeValue a)
                                   where
                                       header = getHeader (tranpose a)
                                       --fdata = getdata header a
                        Left a -> Left a
                   where  
                        output= checkIntegrity (removeEmptyLines a) filename
                                       
        Left  a -> Left (show a)
                                            
                                            
toattributeValue::[[String]] -> [[AttributeValue]]
toattributeValue (x:xs) dataType = (zipWith toattrVal dataType x):(toattributeValue xs)
toattributeValue [] = []


toattrVal:: String -> AttributeValue
toattrVal dataType x =
    case dataType of 
         Numeric =  NumericValue (read x)::Double
         Nominal a = NominalValue (pack x)
         

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
                        Left a -> return (Nominal (getNominalList x []))
                        Right a -> return Numeric
                        

examineFeature :: [String] -> IO (Either Exception.SomeException [a1])
examineFeature (x:xs) = 
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
uniqueInsert x y = if ((ispresent x y) == True) then x
                   else (x++ [pack y])
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
