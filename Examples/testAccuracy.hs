import Classifier.NBClassifier

call trainfile testfile = do 
                            classifier <- buildNBClassifier trainfile
                            accuracy <- testClassifier classifier testfile
			    case accuracy of 
				Right y -> print y
				Left x -> putStrLn x
                            return ()
