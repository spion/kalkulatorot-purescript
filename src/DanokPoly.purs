module DanokPoly where

import Prelude

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

licnoOsloboduvanje :: Number
licnoOsloboduvanje = 8788.0

od :: Number -> Poly -> Poly
od percent exp = percent :*: exp


brutoPoly :: { bruto :: Poly
, brutoMinusPridonesi :: Poly
, danoci :: { dld10 :: Poly
            }
, dldOsnova10 :: Poly
, neto :: Poly
, pridonesi :: { boluvanje :: Poly
               , nevrabotenost :: Poly
               , penzisko :: Poly
               , zdravstveno :: Poly
               }
, vkupnoDavacki :: Poly
, vkupnoPridonesi :: Poly
}
brutoPoly = mkPoly \bruto ->
  let

    pridonesi = { penzisko: bruto # od procentiPridonesi.penzisko
    , zdravstveno: bruto # od procentiPridonesi.zdravstveno
    , nevrabotenost: bruto # od procentiPridonesi.nevrabotenost
    , boluvanje: bruto # od procentiPridonesi.boluvanje
    }

    vkupnoPridonesi = pridonesi.penzisko
      :+: pridonesi.zdravstveno
      :+: pridonesi.nevrabotenost
      :+: pridonesi.boluvanje


    dldOsnova =
        bruto :-: vkupnoPridonesi :-: (pnum licnoOsloboduvanje)

    danoci = { dld10: od procentiDanoci.dld10 dldOsnova }

    vkupnoDanoci = danoci.dld10

    neto =
        bruto :-: vkupnoPridonesi :-: vkupnoDanoci

  in { bruto: bruto
    , neto: neto
    , pridonesi: pridonesi
    , danoci: danoci
    , dldOsnova10: dldOsnova
    , vkupnoDavacki: vkupnoPridonesi :+: vkupnoDanoci
    , vkupnoPridonesi: vkupnoPridonesi
    , brutoMinusPridonesi: bruto :-: vkupnoPridonesi
    }


bruto2neto = toFunction brutoPoly.neto

bruto2netoInverse = toInverseFunction brutoPoly.neto