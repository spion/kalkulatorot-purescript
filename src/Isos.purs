module Isos (x) where


import Prelude

import Data.Either (Either(..))
import Data.Lens ((^.))
import Data.Lens.Iso (iso)
import Data.Lens.Types (Iso')
import Data.Maybe (Maybe(..))


fromMaybe :: forall a. Maybe a -> Either Unit a
fromMaybe (Just x) = Right x
fromMaybe Nothing = Left unit

fromEither :: forall a. Either Unit a -> Maybe a
fromEither (Left _) = Nothing
fromEither (Right a) = Just a

someIso :: forall a. Iso' (Maybe a) (Either Unit a)
someIso = iso fromMaybe fromEither

x :: Either Unit String
x = Just "hi" ^. someIso