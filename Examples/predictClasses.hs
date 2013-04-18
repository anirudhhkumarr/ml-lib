import Classifier.NBClassifier

call trainfile testfile = do 
                            classifier <- buildNBClassifier trainfile
                            classes <- predictClasses classifier testfile
			    case classes of
				Right x -> print x
				Left y -> putStrLn y 
                            return ()

