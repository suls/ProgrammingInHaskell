-- | Main entry point to the application.
module ChapterFive where

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k m = [v | (k', v) <- m, k == k']

pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted list = and [x < y | (x, y) <- pairs list ]

-- | The main entry point.
--main :: IO ()
--main = do
--    putStrLn show(primes 100)
