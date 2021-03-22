factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- トリプルの3番目を取り出す
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- 独自のhead関数
head' :: [a] -> a
head' [] = error "Can't call head on empty list."
head' (x:_) = x


--リストの要素を回りくどく出力する
tell :: (Show a) => [a] -> String
tell []        = "The list is empty"
tell (x:[])    = "The list has one element: " ++ show x
tell (x:y:[])  = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)   = "This list is long. The first two elemnts are: " ++ show x ++ " and " ++ show y

-- asパターン
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- ガード, whereキーワード
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= underweight = "UNDERWEIGHT"
    | bmi <= normal      = "NORMAL"
    | bmi <= height      = "FAT"
    | otherwise          = "WHALE"
    where bmi = weight / height ^ 2
          underweight = 18.5
          normal      = 25.0
          height      = 30.0

-- 独自のmax, compare
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b     = EQ
    | a <= b     = LT
    | otherwise  = GT

-- パターンマッチとwhere
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- whereブロックの中の関数
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- let
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in sideArea + 2 * topArea
