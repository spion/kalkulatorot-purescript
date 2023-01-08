module InvertablePoly
  ( (*:)
  , (:*)
  , (:+:)
  , (:-:)
  , Constituent(..)
  , Order(..)
  , Poly(..)
  , mkPoly
  , padd
  , pmul
  , pmulFlipped
  , pnum
  , psub
  , toFunction
  , toInverseFunction
  ) where

import Prelude
import Data.Foldable (sum)
import Data.List (List(..), filter, (:))

-- No support to invert higher order polynomals
data Order
  = Zero
  | One

derive instance eqOrder :: Eq Order

derive instance ordOrder :: Ord Order

data Constituent = Constituent Order Number

orderOf :: Constituent -> Order
orderOf (Constituent ord _) = ord

multiplierOf :: Constituent -> Number
multiplierOf (Constituent _ nn) = nn

data Poly = Poly (List Constituent)

padd :: Poly -> Poly -> Poly
padd (Poly p1) (Poly p2) = Poly (p1 <> p2)

infixr 6 padd as :+:

pnum :: Number -> Poly
pnum a = Poly (Constituent Zero a : Nil)

cmul :: Number -> Constituent -> Constituent
cmul a (Constituent ord nn) = Constituent ord (a * nn)

pmul :: Number -> Poly -> Poly
pmul a (Poly l) = Poly (map (cmul a) l)

infixr 4 pmul as *:

pmulFlipped :: Poly -> Number -> Poly
pmulFlipped = flip pmul

infixl 4 pmulFlipped as :*

psub :: Poly -> Poly -> Poly
psub p1 p2 = padd p1 (pmul (-1.0) p2)

infixl 6 psub as :-:

x :: Poly
x = Poly $ (Constituent One 1.0) : Nil

polyToMultipliers :: Poly -> { a :: Number, b :: Number }
polyToMultipliers (Poly poly) =
  let
    xes = (filter (eq One <<< orderOf) poly)

    consts = (filter (eq Zero <<< orderOf) poly)

    a = sum (map multiplierOf xes)

    b = sum (map multiplierOf consts)
  in
    { a, b }

toFunction :: Poly -> Number -> Number
toFunction poly =
  let
    { a, b } = polyToMultipliers poly

    fn xVal = a * xVal + b
  in
    fn

toInverseFunction :: Poly -> Number -> Number
toInverseFunction poly =
  let
    { a, b } = polyToMultipliers poly

    fn yVal = (yVal - b) / a
  in
    fn

mkPoly :: forall a. (Poly -> a) -> a
mkPoly fn = fn x
