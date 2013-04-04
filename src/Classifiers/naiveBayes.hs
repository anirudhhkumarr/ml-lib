module NaiveBayes (train, test) where

data Classifier = Classifier{ mu :: Float
														, sigma :: Float
														, n :: Int
														} deriving (Show)

data Attr = N Float
					| C Char

data TrainData = [([Attr], Int)]

data TestData = [[Attr]]

train _ [] = train [] [(0.0,0.0,0)]
train [] clf = clf
train ((attr, cls):xs) clf = train xs clf' 
														 			where
																		classifier = getIth clf cls
																		mu' = mu classifier
																		sigma' = sigma classifier
																		n' = n classifier
																		mean = mu' + (attr - mu')/n'
																		std = sigma' + (attr - sigma')*(attr - mean)
																		clf' = updateIth clf cls (mean, std, n'+1)
