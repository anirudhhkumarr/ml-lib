module NBClassifier where

import Text.ARFF as ARFF
import Test
import Train
import System.IO  
import Control.Monad

--Data.ByteString
import Data.ByteString.Char8(pack,unpack)
import qualified Data.ByteString as BS


--Data.Maybe
import Data.Maybe(catMaybes)

--Data.Map
import qualified Data.Map as Map

--Data.List
import Data.List

--Data.Function
import Data.Function


getdifference::[AttributeValue]->[BS.ByteString]->Int
getdifference ((NominalValue x):xs) (y:ys) = if x == y then getdifference xs ys else 1+getdifference xs ys
getdifference [] [] = 0
classify x y = do
		(header,classifier)<- train x
		--print classifier
		(originClasses,learnedClasses) <- test (header,classifier) y
		print learnedClasses		
		print $ getdifference originClasses learnedClasses
		return ()
