module Ingscale.Util where

import qualified Data.Text.Lazy as T (pack, Text)

-------------------------------
-- General Utility Functions --
-------------------------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a

showText :: Show a => a -> T.Text
showText = T.pack . show
