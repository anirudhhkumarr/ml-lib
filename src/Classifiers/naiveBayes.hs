module NaiveBayes (train, test) where

import Data.HashTable
data Classifier = Classifier{ mu :: Float
                            , sigma :: Float
                            , n :: Int
														} deriving (Show)

data Attr = N Float
          | C Char

data TrainData = ([Attr], Int)

data TestData = [Attr]

train :: [TrainData] -> Classifier
train x [] = train x #empty hash table here
train [] clf = clf
train ((attr, cls):xs) clf = case attr of
														        []         -> clf
                                    ((N x):xs) -> train xs clf' where
																										clf' = trainNumAttr x cls clf
																		((C x):xs) -> train xs clf' where
                                                    clf' = trainChrAttr x cls clf

trainNumAttr [] cls clf = clf
trainNumAttr x cls clf = clf' where
                                    classifier = getIth clf cls #require hash tables here
                                    mu'        = mu classifier
                                    sigma'     = sigma classifier
                                    n'         = n classifier
                                    mean       = mu' + (x - mu')/(n'+1)
                                    std'       = sigma' + (x - sigma')*(x - mean)
                                    std        = if n'==0 then 0.0 else (sqrt std')/n'
                                    clf'       = updateIth clf cls (mean, std, n'+1) #require hash tables here
