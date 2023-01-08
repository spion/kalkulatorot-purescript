module DanokPoly where

import Prelude
import Heterogeneous.Mapping (hmap)
import InvertablePoly (Poly, mkPoly, pnum, toFunction, toInverseFunction, (:*:), (:+:), (:-:))

procentiPridonesi
  ∷ { boluvanje ∷ Number, nevrabotenost ∷ Number, penzisko ∷ Number, zdravstveno ∷ Number }
procentiPridonesi =
  { penzisko: 0.188
  , zdravstveno: 0.075
  , nevrabotenost: 0.012
  , boluvanje: 0.005
  }

procentiDanoci :: { dld10 :: Number }
procentiDanoci = { dld10: 0.1 }

licnoOsloboduvanje :: Number
licnoOsloboduvanje = 8788.0

prosecnaPlata :: Number
prosecnaPlata = 43509.0

maxOsnovica :: Number
maxOsnovica = prosecnaPlata * 16.0

type Results a =
  { bruto :: a
  , brutoMinusPridonesi :: a
  , danociDld10 :: a
  , dldOsnova10 :: a
  , neto :: a
  , pridonesiBoluvanje :: a
  , pridonesiNevrabotenost :: a
  , pridonesiPenzisko :: a
  , pridonesiZdravstveno :: a
  , vkupnoDavacki :: a
  , vkupnoPridonesi :: a
  }

od :: Number -> Poly -> Poly
od percent exp = percent :*: exp

brutoPolys :: Results Poly
brutoPolys =
  mkPoly \bruto ->
    let
      pridonesiPenzisko = procentiPridonesi.penzisko :*: bruto

      pridonesiZdravstveno = procentiPridonesi.zdravstveno :*: bruto

      pridonesiNevrabotenost = procentiPridonesi.nevrabotenost :*: bruto

      pridonesiBoluvanje = procentiPridonesi.boluvanje :*: bruto

      vkupnoPridonesi =
        pridonesiPenzisko
          :+: pridonesiZdravstveno
          :+: pridonesiNevrabotenost
          :+: pridonesiBoluvanje

      dldOsnova = bruto :-: vkupnoPridonesi :-: (pnum licnoOsloboduvanje)

      danociDld10 = procentiDanoci.dld10 :*: dldOsnova

      vkupnoDanoci = danociDld10

      neto = bruto :-: vkupnoPridonesi :-: vkupnoDanoci
    in
      { bruto
      , neto
      , pridonesiPenzisko
      , pridonesiZdravstveno
      , pridonesiNevrabotenost
      , pridonesiBoluvanje
      , danociDld10: danociDld10
      , dldOsnova10: dldOsnova
      , vkupnoDavacki: vkupnoPridonesi :+: vkupnoDanoci
      , vkupnoPridonesi: vkupnoPridonesi
      , brutoMinusPridonesi: bruto :-: vkupnoPridonesi
      }

brutoPFunctions :: Results (Number -> Number)
brutoPFunctions = hmap toFunction brutoPolys

applyWithArg :: Number -> (Number -> Number) -> Number
applyWithArg arg f = f arg

type Model = Results Number

bruto2neto :: Number -> Model
bruto2neto bruto = hmap (applyWithArg bruto) brutoPFunctions

bruto2netoInverse ∷ Number → Number
bruto2netoInverse = toInverseFunction brutoPolys.neto

neto2bruto ∷ Number -> Model
neto2bruto = bruto2neto <<< bruto2netoInverse
