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

parseARFF input = parse arff input    

sortByClass xs n = sortBy (compare `on` (!!(n-1))) xs  

main = do
        handle <- openFile "arff.arff" ReadMode
        contents <- hGetContents handle
        let input = case (parseARFF $ pack contents) of Partial k -> (k BS.empty)
                     			    	        x-> x
	let (_header,_data) = case input of Done _ y ->y
	let inputData = map catMaybes _data	
	    numAttributes = length $ attributes _header
	    Nominal classes = dataType $ last $ attributes _header
	    attributesInfo = attributes _header	
	    sortedInput = sortByClass inputData numAttributes	    
	print sortedInput	
	return ()
