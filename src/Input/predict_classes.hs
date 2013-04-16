import NBClassifier

main = do 
	classifier <- buildNBClassifier "train_arff.arff"
	classes <- predictClasses classifier "predict_classes.arff"
	print classes
	return ()
