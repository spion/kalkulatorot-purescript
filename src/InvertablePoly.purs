module InvertablePoly
  ( (:*:)
  , (:+:)
  , (:-:)
  , Constituent(..)
  , Order(..)
  , Poly(..)
  , mkPoly
  , pnum
  , padd
  , pmul
  , psub
  , toFunction
  , toInverseFunction
  )
  where

import Prelude

import Data.Foldable (sum)
import Data.List (List(..), filter, (:))


-- No support to invert higher order polynomals
data Order = Zero | One

derive instance eqOrder :: Eq Order
derive instance ordOrder :: Ord Order


data Constituent = Constituent Order Number

orderOf :: Constituent -> Order
orderOf (Constituent ord _) = ord

multiplierOf :: Constituent -> Number
multiplierOf (Constituent _ nn) = nn

data Poly = Poly (List Constituent)


instance Semigroup Poly where
  append (Poly p1) (Poly p2) = Poly (p1 <> p2)

instance Monoid Poly where
  mempty = Poly Nil


padd :: Poly -> Poly -> Poly
padd = append

infixr 6 padd as :+:

pnum :: Number -> Poly
pnum a = Poly (Constituent Zero a : Nil)


cmul :: Number -> Constituent -> Constituent
cmul a (Constituent ord nn) = Constituent ord (a * nn)

pmul :: Number -> Poly -> Poly
pmul a (Poly l) = Poly (map (cmul a) l)

infixr 4 pmul as :*:

psub :: Poly -> Poly -> Poly
psub p1 p2 = append p1 (pmul (-1.0) p2)

infixr 6 psub as :-:

x :: Poly
x = Poly $ (Constituent One 1.0) : Nil


polyToMultipliers :: Poly -> {a :: Number, b:: Number}

polyToMultipliers (Poly poly) =
  let
    xes = (filter (eq One <<< orderOf) poly)
    consts = (filter (eq Zero <<< orderOf) poly)

    a = sum (map multiplierOf xes)
    b = sum (map multiplierOf consts)

  in {a, b}

toFunction :: Poly -> Number -> Number

toFunction poly =
  let
    {a, b} = polyToMultipliers poly
    fn xVal = a * xVal + b

  in fn


toInverseFunction :: Poly -> Number -> Number
toInverseFunction poly =
  let
    {a, b} = polyToMultipliers poly
    fn yVal = (yVal - b) / a

  in fn


mkPoly :: forall a. (Poly -> a) -> a
mkPoly fn = fn x