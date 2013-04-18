import Classifier.NBClassifier

call:: String -> IO ()
call file = do 
                    classifier <- buildNBClassifier file
                    case classifier of 
                        Right a -> 
                            do
                                putStrLn "\nHeader\n"
                                print $ fst a
                                putStrLn "\nData\n"
                                print $ snd a
                        Left a -> putStrLn a

                    return ()
