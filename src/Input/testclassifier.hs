import NBClassifier

main = do 
	classifier <- buildNBClassifier "train_arff.arff"
	accuracy <- testClassifier classifier "test_arff.arff"
	print accuracy
	return ()
