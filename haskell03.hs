-- Paradigmas
-- Nome : Renan de Siqueira Cecchin

add10toall :: [Int] -> [Int]
add10toall y = [x + 10 | x <- y]

multN :: Int -> [Int] -> [Int]
multN a y = [x + a | x <- y]

multN' :: Int -> [Int] -> [Int]
multN' a x = map (\x -> x + a) x

applyExpr :: [Int] -> [Int]
applyExpr y = [3 * x + 2 | x <- y]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map (\x -> 3 * x + 2) x

addSuffix :: String -> [String] -> [String]
addSuffix str y = [x ++ str| x <- y]

selectgt5 :: [Int] -> [Int]
selectgt5 y = [x | x <- y, x > 5]

sumOdds :: [Int] -> Int
sumOdds y = sum ([x | x <- y, x `mod`2 == 1])

sumOdds' :: [Int] -> Int
sumOdds' x = sum (filter even x)

selectExpr :: [Int] -> [Int]
selectExpr x = [x | x <- x, x `mod`2 == 0]

countShorts :: [String] -> Int
countShorts x = length [x | x <- x, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr x = [x^2/2 | x <- x, x > 10]

trSpaces :: String -> String
trSpaces y = [if x == ' ' then '-' else x | x <- y]

selectSnd :: [(Int, Int)] -> [Int]
selectSnd y = [snd x | x <- y]

dotProd :: [Int] -> [Int] -> Int
dotProd y z = sum([fst x * snd x | x <- zip y z])