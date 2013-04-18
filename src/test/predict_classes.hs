import NBClassifier

main = do 
	classifier <- buildNBClassifier "train_csv.csv"
	classes <- predictClasses classifier "predict_classes.csv"
	print classes
	return ()
