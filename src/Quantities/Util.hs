module Quantities.Util 
       ( maybeToEither
       , eitherAlternative
       , showText
       ) where

import qualified Data.Text.Lazy as T (pack, Text)

-------------------------------
-- General Utility Functions --
-------------------------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a

-- | Return the result of `show' as a (lazy) Text.
showText :: Show a => a -> T.Text
showText = T.pack . show

-- | Return first Right value in the list. If the list contains to
-- Right, return default Left.
eitherAlternative :: a -> [Either a b] -> Either a b
eitherAlternative def []             = Left def
eitherAlternative _   (Right x : _)  = Right x
eitherAlternative def (Left _  : xs) = eitherAlternative def xs
