module Brainfuck
    ( BfSource(..)
    , BfCommand(..)
    , parseBf
    , fromBfSource
    ) where

import Data.Maybe (mapMaybe)
import TwoOrLess

data BfCommand = GoLeft Int
               | GoRight Int
               | Add Int
               | Sub Int
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

newtype BfSource = BfSource [BfCommand] deriving (Show)

fromBfSource (BfSource x) = x

-- instance Show BfSource where
--     show (BfSource []) = "empty"
--     show (BfSource bs) = concatMap show bs

pairLoops :: [Int] -> [BfCommand] -> [BfCommand] -> [BfCommand]
pairLoops _      q []           = reverse q
pairLoops st     q (LoopL x:bs) = pairLoops (x:st) (LoopL x : q) bs
pairLoops (s:st) q (LoopR _:bs) = pairLoops st (LoopR s : q) bs
pairLoops st     q (b:bs)       = pairLoops st (b : q) bs

parseBf :: String -> BfSource
parseBf =
    optimiseBf . BfSource .
    pairLoops [] [] . countLoopLs 0 .
    reduceConsts . mapMaybe char2bfc
  where
    char2bfc :: Char -> Maybe BfCommand
    char2bfc '<' = Just $ GoLeft 1
    char2bfc '>' = Just $ GoRight 1
    char2bfc '+' = Just $ Add 1
    char2bfc '-' = Just $ Sub 1
    char2bfc '[' = Just $ LoopL 0
    char2bfc ']' = Just $ LoopR 0
    char2bfc '.' = Just WriteChar
    char2bfc ',' = Just ReadChar
    char2bfc _   = Nothing
    countLoopLs :: Int -> [BfCommand] -> [BfCommand]
    countLoopLs _ [] = []
    countLoopLs n (LoopL _:bs) = LoopL n : countLoopLs (n + 1) bs
    countLoopLs n (b:bs) = b : countLoopLs n bs
    reduceConsts :: [BfCommand] -> [BfCommand]
    reduceConsts [] = []
    reduceConsts (LoopL _:Sub 1:LoopR _:bs) = BfConst 0 : reduceConsts bs
    reduceConsts (LoopL _:Add 1:LoopR _:bs) = BfConst 0 : reduceConsts bs
    reduceConsts (b:bs) = b : reduceConsts bs


-- performance is probably horrible,
-- but I want to focus on simplicity
optimiseBf :: BfSource -> BfSource
optimiseBf (BfSource bs) =
    if bs /= obs
        then optimiseBf (BfSource obs)
        else BfSource obs
  where
    obs = opthelper bs
    opthelper :: [BfCommand] -> [BfCommand]
    opthelper []  = []
    opthelper [x] = [x]
    opthelper (x:y:xs) =
        let r        = reduceBf x y
            single   = fromOne r
            (s1, s2) = fromTwo r
         in case r of
                Zero      -> opthelper xs
                (One _)   -> single : opthelper xs
                (Two _ _) -> s1 : opthelper (s2 : xs)
--------------------
reduceBf :: BfCommand -> BfCommand -> TwoOrLess BfCommand
reduceBf (Add a)     (Add b)     = One . Add     $ a + b
reduceBf (Sub a)     (Sub b)     = One . Sub     $ a + b
reduceBf (GoLeft a)  (GoLeft b)  = One . GoLeft  $ a + b
reduceBf (GoRight a) (GoRight b) = One . GoRight $ a + b
reduceBf (Add a)     (BfConst b) = One . BfConst $ b
reduceBf (Sub a)     (BfConst b) = One . BfConst $ b
reduceBf (BfConst a) (BfConst b) = One . BfConst $ b
reduceBf (BfConst a) (Add b)     = One . BfConst $ a + b
reduceBf (BfConst a) (Sub b)     = One . BfConst $ a - b

reduceBf (Add a) (Sub b)
    | a > b = One . Add $ a - b
    | a < b = One . Sub $ b - a
    | otherwise = Zero

reduceBf (Sub a) (Add b)
    | a > b = One . Sub $ a - b
    | a < b = One . Add $ b - a
    | otherwise = Zero

reduceBf (GoLeft a) (GoRight b)
    | a > b = One . GoLeft  $ a - b
    | a < b = One . GoRight $ b - a
    | otherwise = Zero

reduceBf (GoRight a) (GoLeft b)
    | a > b = One . GoRight $ a - b
    | a < b = One . GoLeft  $ b - a
    | otherwise = Zero

reduceBf a b = Two a b
------------------