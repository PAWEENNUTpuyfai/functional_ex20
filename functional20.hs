import Text.Read
import System.Random 
--------code form lec20---------
--  improve your number-guessing game to 
-- randomize the desired number
--  can you reuse code from the previous 
-- version?

--นำ read target ออก เพิ่ม randNum สำหรับ random number

-- readTarget :: IO Int
-- readTarget = readNumber "Target number"

randNum :: RandomGen g => g -> Int
randNum gen = fst $ uniform gen

readNumber :: [Char] -> IO Int
readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

verdict :: Ord a => a -> a -> Either String String
verdict target guess = do
    case compare guess target of
        EQ -> Right "You win!"
        LT -> Left "Too low"
        GT -> Left "Too high"


runGameV3 :: (Ord t, Num t) => Int -> t -> t -> IO ()
runGameV3 num lim count = do
    guess <- readNumber "Guess"
    let v = verdict num guess
    case v of
        Right m -> do
            putStrLn m
        Left m -> do
            putStrLn m
            if count < lim
            then runGameV3 num lim (count+1)
            else putStrLn "Game over"


game :: IO ()
game = do
    g <- newStdGen
    let target = randNum g
    -- num <- readTarget
    lim <- readNumber "Guess limit"
    runGameV3 target lim 1


{-improve your number-guessing game to reject impossible guesses
    if the number entered is at least another guess that's already higher than the target
    or if the number entered is at most another guess that's already lower than the target
    don't count these guesses against the limit
-}
--add rejectImpossible and change runGame

rejectImpossible :: Int -> Maybe Int -> Maybe Int -> Int -> Bool
rejectImpossible guess lowerBound upperBound target =
    (lowerBound /= Nothing && guess <= (maybe 0 id lowerBound) && guess < target) ||
    (upperBound /= Nothing && guess >= (maybe 0 id upperBound) && guess > target)


runGameV4 :: Int -> Int -> Int -> Maybe Int -> Maybe Int -> IO ()
runGameV4 target lim count lowerBound upperBound = do
    guess <- readNumber "Guess"
    if rejectImpossible guess lowerBound upperBound target then do
        putStrLn "Impossible guess! Try again."
        runGameV4 target lim count lowerBound upperBound 
    else do
        let v = verdict target guess
        case v of
            Right m -> do
                putStrLn m
                putStrLn $ "The correct number was: " ++ show target 
            Left m  -> do
                putStrLn m
                if count < lim
                    then runGameV4 target lim (count + 1) 
                        (if guess < target then Just guess else lowerBound)
                        (if guess > target then Just guess else upperBound)
                    else do
                        putStrLn "Game over"
                        putStrLn $ "The correct number was: " ++ show target  

gameV4 :: IO ()
gameV4 = do
    g <- newStdGen
    let target = randNum g
    -- num <- readTarget
    lim <- readNumber "Guess limit"
    runGameV4 target lim 1 Nothing Nothing



{- write function nRandomRs that generates a 
list of n random values, each in a given 
range
 nRandomRs :: (RandomGen g,
  UniformRange a, Integral n)
    => (a, a) -> n -> g -> ([a], g)-}

nRandomRs :: (RandomGen g,
  UniformRange a, Integral n)
    => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs range n gen =
 let (val, gen') = uniformR range gen
     (rest, gen'') = nRandomRs range (n-1) gen'
 in (val:rest, gen'') 
