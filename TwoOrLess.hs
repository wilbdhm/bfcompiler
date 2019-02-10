module TwoOrLess
    ( TwoOrLess(..)
    , fromOne
    , fromTwo
    ) where

data TwoOrLess a = Zero
                 | One a
                 | Two a a
                 deriving (Show, Eq)

fromTwo :: TwoOrLess a -> (a, a)
fromTwo (Two x y) = (x, y)
fromTwo (One _) = error "fromTwo: One"
fromTwo _ = error "fromTwo: Zero"

fromOne :: TwoOrLess a -> a
fromOne (One x) = x
fromOne Zero = error "fromOne: Zero"
fromOne _ = error "fromOne: Two"