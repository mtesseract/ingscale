module Ingscale.Util where

-------------------------------
-- General Utility Functions --
-------------------------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a
