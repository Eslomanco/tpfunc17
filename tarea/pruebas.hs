type Name = String


conteo :: Eq a => a -> [a] -> Int
conteo x [] = 0
conteo x (y:ys) | x==y = 1 + (conteo x ys)
                | otherwise = conteo x ys

auxDupFunc :: [Name] -> [Name] -- Genera lista de duplicados
auxDupFunc x = [y | y <- x, (conteo y x) > 1]
