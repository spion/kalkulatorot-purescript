module ConditionalInvertablePoly
  ( (:<=)
  , polyLteNumber
  , CondPoly
  , Limit
  ) where

import Prelude

import Data.List (List, dropWhile, filter, takeWhile, (:))
import Data.Maybe (Maybe(..), isJust)
import InvertablePoly (Poly(..))

data Limit = Limit Number Poly
-- Limit xLimit purePoly

data CondPoly = CondPoly (List Limit)

-- cond (neshoPlataAU :<= maxPlata) (konst1 * nestoPlataAU) (konst2 * nestoPlataAU)

-- if we do this with Limit which is Limit Number CondPoly
-- then we find the number in the condpoly list

-- (ax + b) <= number --> x <= (number - b) / a
-- (caveat, what if a is negative, inverzija na neravinstvo) TODO
limitToXLimit :: Number -> Poly -> Number
limitToXLimit number (Poly a b) = (number - b) / a

validLimitToXLimit :: Number -> Limit -> Maybe Number
validLimitToXLimit polyLimit (Limit xLim poly) =
  let
    calculatedLimit = limitToXLimit polyLimit poly
  in
    if calculatedLimit <= xLim then Just calculatedLimit else Nothing

limitToXLimitPoly :: Partial => CondPoly -> Number -> Number
limitToXLimitPoly (CondPoly polys) polyLimit =
  let
    allXLimits = (map (validLimitToXLimit polyLimit) polys)
    (Just first : _) = (filter isJust allXLimits)
  in
    first

-- (condPoly + condPoly)
-- x to N1, p1, x to N2, p2, N2 to inf -> p3
-- [---p1----|---p2-----------|----p3---]
-- [------p4---------|----------p5------]
-- [ p1+p4   | p2+p4 | p2+p5  | p3+p5   ]

-- cond (condPoly) will take part of the conditional poly below the condition limit
-- and part of the other conditional poly above the other limit, then concat those lists

-- TODO: TEST THIS WITH REAL CONDITIONAL POLY AS INPUT
polyLteNumber :: Partial => CondPoly -> Number -> CondPoly -> CondPoly -> CondPoly

polyLteNumber condPoly compareNumber (CondPoly ifTrueLimits) (CondPoly ifFalseLimits) =
  let
    newLimit = limitToXLimitPoly condPoly compareNumber
    lessThanLimit (Limit limitNum _) = limitNum <= newLimit
    originalTrueConstituents = takeWhile lessThanLimit ifTrueLimits
    (firstTrueConstituent : _) = dropWhile lessThanLimit ifTrueLimits
  -- then take the next from ifTrueLimits, but with newLimit
  -- then take all limits > newLimit
  in
    (CondPoly originalTrueConstituents)

-- first we need to create a limit based on CondPoly and Number
-- (ax + b) <= number --> x <= (number - b) / a
-- the question is which of the polynomals to use to find the X limit?
-- for each of the polys, calculate xLimit = (compareNumber - b) / 2
-- if xLimit is less than this Limit's number
-- then we can use that poly to calculate the xLimit for the new CondPoly
-- we're making
-- otherwise continue through the list until you find the right spot
-- once we find the xLimit
-- the result is (List (Limit xLimit ifTruePoly) (Limit xLimit ifFalsePoly) : nil

infixr 3 polyLteNumber as :<=

