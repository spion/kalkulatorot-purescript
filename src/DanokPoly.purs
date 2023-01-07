module DanokPoly where

import Prelude

import Data.Function (applyFlipped)
import Heterogeneous.Mapping (hmap)
import InvertablePoly (Poly, mkPoly, pnum, toFunction, toInverseFunction, (:*:), (:+:), (:-:))

procentiPridonesi ∷ { boluvanje ∷ Number , nevrabotenost ∷ Number , penzisko ∷ Number , zdravstveno ∷ Number }
procentiPridonesi =
    { penzisko: 0.188
    , zdravstveno: 0.075
    , nevrabotenost: 0.012
    , boluvanje: 0.005
    }

procentiDanoci :: { dld10 :: Number }
procentiDanoci =
    { dld10: 0.1 }


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

licnoOsloboduvanje :: Number
licnoOsloboduvanje = 8788.0

od :: Number -> Poly -> Poly
od percent exp = percent :*: exp


brutoPolys :: Results Poly
brutoPolys = mkPoly \bruto ->

  let
    pridonesiPenzisko = bruto # od procentiPridonesi.penzisko
    pridonesiZdravstveno = bruto # od procentiPridonesi.zdravstveno
    pridonesiNevrabotenost = bruto # od procentiPridonesi.nevrabotenost
    pridonesiBoluvanje = bruto # od procentiPridonesi.boluvanje


    vkupnoPridonesi = pridonesiPenzisko
      :+: pridonesiZdravstveno
      :+: pridonesiNevrabotenost
      :+: pridonesiBoluvanje


    dldOsnova =
        bruto :-: vkupnoPridonesi :-: (pnum licnoOsloboduvanje)

    danociDld10 = dldOsnova # od procentiDanoci.dld10

    vkupnoDanoci = danociDld10

    neto =
        bruto :-: vkupnoPridonesi :-: vkupnoDanoci

  in { bruto
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

bruto2neto :: Number -> Results Number
bruto2neto bruto = hmap (applyWithArg bruto) brutoPFunctions

bruto2netoInverse ∷ Number → Number
bruto2netoInverse = toInverseFunction brutoPolys.neto
neto2bruto  ∷ Number -> Results Number
neto2bruto = bruto2neto <<< bruto2netoInverse