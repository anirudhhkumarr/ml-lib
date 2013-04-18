import Classifier.NBClassifier
main = do 
				classifier <- buildNBClassifier "DiabetesDiagnosis-train.arff"
				accuracy <- testClassifier classifier "DiabetesDiagnosis-train.arff"
				classes <- predictClasses classifier "DiabetesDiagnosis-test.arff"
				print classes
				print accuracy
				return ()
