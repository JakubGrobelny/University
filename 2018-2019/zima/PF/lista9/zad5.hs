sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = [x:sublists | sublists <- sublist xs] ++ sublist xs
