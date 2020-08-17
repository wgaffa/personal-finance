module Utility.Absolute 
    ( AbsoluteValue()
    , absoluteValue
    , unAbsoluteValue
    ) where

-- | Absolute value container for any absolute number, see constructor 'absoluteValue'
newtype AbsoluteValue a = AbsoluteValue { unAbsoluteValue :: a }
    deriving (Ord, Eq, Show)

instance (Num a) => Semigroup (AbsoluteValue a) where
    (AbsoluteValue a) <> (AbsoluteValue b) = AbsoluteValue (a + b)

instance (Num a) => Monoid (AbsoluteValue a) where
    mempty = AbsoluteValue 0

-- | Constructor for 'AbsoluteValue'
-- This creates an absolute value buy applying 'Prelude.abs' to the argument
absoluteValue :: (Num a) => a -> AbsoluteValue a
absoluteValue = AbsoluteValue . abs
