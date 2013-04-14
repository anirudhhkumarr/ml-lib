module Input (parseARFF) where 

import Text.ARFF as ARFF
import System.IO  
import Control.Monad


--Data.Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as Text


------------------------------------------------------------------------------------------------------------
--parseARFF function takes input file in ByteString form and parses it using arff parser.This function retruns:
--(Fail !ByteString [String] String) => The parse failed. The ByteString is the input that had not yet been consumed when the failure occurred. The 						[String] is a list of contexts in which the error occurred. The String is the message describing the error, 						if any.
--Partial (ByteString -> Result r)   => Supply this continuation with more input so that the parser can resume. To indicate that no more input is 						available, use an empty string.
--Done !ByteString r		     =>	The parse succeeded. The ByteString is the input that had not yet been consumed (if any) when the parse 					succeeded.
		
parseARFF input = parse arff input    
------------------------------------------------------------------------------------------------------------


