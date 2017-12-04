module Utils.Newtype
  (
    over'

  , module Control.Newtype
  ) where

import Control.Newtype

-- | A version of 'over' that doesn't take in a pack function
over'
  :: (Newtype n o, Newtype n' o')
  => (o -> o') -> (n -> n')
over' = over pack
