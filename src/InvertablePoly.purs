module InvertablePoly
  ( (*:)
  , (:*)
  , (:+:)
  , (:-:)
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

-- neto = a*bruto + b
-- bruto = (neto - b) / a

-- y = a*x + b + a2x + b2 + c * (dx + e)

-- y = (a + a2 + c*d) * x + (b + b2 + c*e)

-- ax + b
data Poly = Poly Number Number

padd :: Poly -> Poly -> Poly
padd (Poly a1 b1) (Poly a2 b2) = Poly (a1 + a2) (b1 + b2)

infixr 6 padd as :+:

pnum :: Number -> Poly
pnum b = Poly 0.0 b

pmul :: Number -> Poly -> Poly
pmul c (Poly a b) = Poly (c * a) (c * b)

infixr 4 pmul as *:

pmulFlipped :: Poly -> Number -> Poly
pmulFlipped = flip pmul

infixl 4 pmulFlipped as :*

psub :: Poly -> Poly -> Poly
psub p1 p2 = padd p1 (pmul (-1.0) p2)

infixl 6 psub as :-:

x :: Poly
x = Poly 1.0 0.0

toFunction :: Poly -> Number -> Number
toFunction (Poly a b) =
  let
    fn x = a * x + b
  in
    fn

toInverseFunction :: Poly -> Number -> Number
toInverseFunction (Poly a b) =
  let
    fn y = (y - b) / a
  in
    fn

mkPoly :: forall a. (Poly -> a) -> a
mkPoly fn = fn x
