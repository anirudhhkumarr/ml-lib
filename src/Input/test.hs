import NBClassifier
main = do 
	classifier <- buildNBClassifier "train_arff.arff"
	out <- testClassifier classifier "test_arff.arff"
	--print out
        case out of Left x -> print x
	            Right y ->print y
	return ()
