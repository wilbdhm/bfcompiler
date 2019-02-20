module Brainfuck
    ( BfSource(..)
    , BfCommand(..)
    , parseBf
    ) where

import Data.Maybe (mapMaybe)
import Data.List (uncons, unfoldr, group, mapAccumL)

data BfCommand = GoRight Int
               | Add Int
               | LoopL Int
               | LoopR Int
               | WriteChar
               | ReadChar
               | BfConst Int
               deriving (Eq, Show)

-- instance Show BfCommand where
--     show (GoLeft x)  = replicate x '<'
--     show (GoRight x) = replicate x '>'
--     show (Add x)     = replicate x '+'
--     show (Sub x)     = replicate x '-'
--     show (LoopL x)   = "["
--     show (LoopR x)   = "]"
--     show WriteChar   = "."
--     show ReadChar    = ","
--     show (BfConst x) = show x 

type BfSource = [BfCommand]

parseBf :: String -> BfSource
parseBf =
    optimiseBf .
    snd .
    mapAccumL pairLoops [] .
    snd .
    mapAccumL countLoopLs 0 .
    mapMaybe char2bfc

char2bfc :: Char -> Maybe BfCommand
char2bfc '>' = Just $ GoRight 1
char2bfc '<' = Just $ GoRight (-1)
char2bfc '+' = Just $ Add 1
char2bfc '-' = Just $ Add (-1)
char2bfc '[' = Just $ LoopL undefined
char2bfc ']' = Just $ LoopR undefined
char2bfc '.' = Just WriteChar
char2bfc ',' = Just ReadChar
char2bfc _   = Nothing

countLoopLs :: Int -> BfCommand -> (Int, BfCommand)
countLoopLs n (LoopL _) = (n + 1, LoopL n)
countLoopLs n b = (n, b)

pairLoops :: [Int] -> BfCommand -> ([Int], BfCommand)
pairLoops st     (LoopL x) = (x:st, LoopL x)
pairLoops (s:st) (LoopR _) = (st, LoopR s)
pairLoops st     b         = (st, b)

optimiseBf :: BfSource -> BfSource
optimiseBf = within (/=) . iterate (unfoldr $ uncons . reduceBf)

reduceBf :: [BfCommand] -> [BfCommand]
reduceBf [] = []
reduceBf (Add a : Add b : bs)                = Add (a + b) : bs
reduceBf (GoRight a : GoRight b : bs)        = GoRight (a + b) : bs
reduceBf (BfConst a : Add b : bs)            = BfConst (a + b) : bs
reduceBf (Add a : BfConst b : bs)            = BfConst b : bs
reduceBf (BfConst a : BfConst b : bs)        = BfConst b : bs
reduceBf (LoopL _ : Add   1  : LoopR _ : bs) = BfConst 0 : bs
reduceBf (LoopL _ : Add (-1) : LoopR _ : bs) = BfConst 0 : bs
reduceBf (Add 0 : bs)                        = bs
reduceBf (GoRight 0 : bs)                    = bs
reduceBf bs = bs

within :: (a -> a -> Bool) -> [a] -> a
within f (x:y:xs)
    | f x y     = within f xs
    | otherwise = x