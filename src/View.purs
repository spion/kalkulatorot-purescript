module View (mkMainView) where

import Prelude
import DanokPoly (Model, bruto2neto, licnoOsloboduvanje, neto2bruto, procentiDanoci, procentiPridonesi)
import Data.Int as DI
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString, round)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, Reducer, component, mkReducer, (/\))
import React.Basic.Hooks as React

data Action
  = Bruto String
  | Neto String

showInt :: Number -> String
showInt num = show (DI.round num)

reducerFn :: Model -> Action -> Model
reducerFn model msg = case msg of
  Bruto b -> case fromString b of
    Nothing -> model
    Just bv -> bruto2neto bv
  Neto n -> case fromString n of
    Nothing -> model
    Just nv -> neto2bruto nv

reducerEff :: Effect (Reducer Model Action)
reducerEff = mkReducer reducerFn

-- containerStyle : List (Attribute msg)
containerStyle :: R.CSS
containerStyle =
  R.css
    { width: "600px"
    , left: "50%"
    , marginLeft: "-300px"
    , position: "absolute"
    , verticalAlign: "center"
    , font: "0.8em sans-serif"
    }

bold :: R.CSS
bold = R.css { fontWeight: "bold" }

ribbon :: JSX
ribbon =
  R.a
    { href: "https://github.com/skopjehacklab/kalkulator.ot.mk"
    , children:
        [ R.img
            { style:
                R.css
                  { position: "absolute"
                  , top: "0"
                  , left: "0"
                  , border: "0"
                  }
            , src: "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png"
            , alt: "Fork me on GitHub"
            }
        ]
    }

pdfLinkTxt :: String
pdfLinkTxt = "Закон за данокот на личен доход"

pdfLinkStyle :: R.CSS
pdfLinkStyle =
  R.css
    { float: "right"
    , padding: "10px"
    , fontSize: "12px"
    , backgroundColor: "#fffcda"
    , color: "#000000"
    }

pdfLink :: JSX
pdfLink =
  R.a
    { style: pdfLinkStyle
    , href: "http://ujp.gov.mk/e/regulativa/opis/337"
    , title: pdfLinkTxt
    , target: "_blank"
    , rel: "noopener"
    , children: [ R.text pdfLinkTxt ]
    }

splitter :: R.CSS
splitter =
  R.css
    { marginBottom: "30px"
    , borderBottom: "5px solid #afafaf"
    , borderLeft: "5px solid #afafaf"
    , borderRight: "5px solid #afafaf"
    , padding: "30px 25px"
    , backgroundColor: "#fffcda"
    , width: "600px"
    }

inputStyle :: R.CSS
inputStyle =
  R.css
    { boxSizing: "border-box"
    , lineHeight: "1.25"
    , padding: ".5rem .75rem"
    , backgroundClip: "padding-box"
    , width: "250px"
    , borderRadius: ".25rem"
    , border: "1px solid rgba(0,0,0,.15)"
    }

rowStyle :: { borderBottom :: String, padding :: String, textAlign :: String }
rowStyle =
  { borderBottom: "1px solid #afafaf"
  , padding: "15px"
  , textAlign: "left"
  }

td :: String -> JSX
td txt = R.td { style: R.css (rowStyle { textAlign = "right" }), children: [ R.text txt ] }

tdLeft :: String -> JSX
tdLeft txt = R.td { style: R.css rowStyle, children: [ R.text txt ] }

mkInputFields :: Component { model :: Model, dispatch :: Action -> Effect Unit }
mkInputFields = do
  component "InputFields" \props -> React.do
    pure
      $ R.table
          { style: splitter
          , children:
              [ R.tr_
                  [ R.th_ [ R.text "Бруто" ]
                  , R.th_ [ R.text "Нето" ]
                  ]
              , R.tr_
                  [ R.td_
                      [ R.input
                          { title: "Бруто, износ кој ја вклучува чистата плата што ја добива работникот (нето-плата) заедно со сите јавни давачки (даноци и придонеси), во бруто-платата се вклучени надоместоците кои ги добиваат вработените за храна и за превоз"
                          , type: "number"
                          , placeholder: "Бруто"
                          , onChange: handler targetValue \v -> props.dispatch (Bruto $ fromMaybe "" v)
                          , value: (showInt props.model.bruto)
                          , style: inputStyle
                          }
                      ]
                  , R.td_
                      [ R.input
                          { title: "Нето, чистата плата што ја добива работникот на својата трансакциска сметка"
                          , type: "number"
                          , placeholder: "Нето"
                          , onChange: handler targetValue \v -> props.dispatch (Neto $ fromMaybe "" v)
                          , value: (showInt props.model.neto)
                          , style: inputStyle
                          }
                      ]
                  ]
              ]
          }

infoIcon :: JSX
infoIcon =
  R.span
    { style:
        R.css
          { display: "inline-block"
          , width: "16px"
          , height: "16px"
          , textAlign: "center"
          , borderRadius: "50%"
          , background: "#9898ea"
          , color: "#fff"
          , marginRight: "5px"
          , userSelect: "none"
          }
    , children: [ R.i_ [ R.text "i" ] ]
    }

showPercentage :: Number -> String
showPercentage num = (show $ round (num * 10000.0) / 100.0) <> "%"

details :: Model -> JSX
details model =
  R.div
    { style: R.css { margin: "0 0 50px 0" }
    , children:
        [ R.table_
            [ R.tr
                { style: bold
                , children:
                    [ tdLeft "Бруто"
                    , td ""
                    , td (showInt model.bruto)
                    , td "МКД"
                    ]
                }
            , R.tr_
                [ tdLeft "Придонеси за задолжително ПИО"
                , td (showPercentage procentiPridonesi.penzisko)
                , td (showInt model.pridonesiPenzisko)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Придонеси за задолжително здравствено осигурување"
                , td (showPercentage procentiPridonesi.zdravstveno)
                , td (showInt model.pridonesiZdravstveno)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Придонес за осигурување во случај на невработеност"
                , td (showPercentage procentiPridonesi.nevrabotenost)
                , td (showInt model.pridonesiNevrabotenost)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , td (showPercentage procentiPridonesi.boluvanje)
                , td (showInt model.pridonesiBoluvanje)
                , td "МКД"
                ]
            , R.tr
                { style: bold
                , children:
                    [ tdLeft "Вкупно придонеси"
                    , td ""
                    , td (showInt model.vkupnoPridonesi)
                    , td "МКД"
                    ]
                }
            , R.tr_
                [ tdLeft "Бруто плата намалена за придонеси"
                , td ""
                , td (showInt model.brutoMinusPridonesi)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Лично ослободување"
                , td ""
                , td (showInt licnoOsloboduvanje)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Даночна основа за пресметка на данок на личен доход"
                , td ""
                , td (showInt model.dldOsnova10)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Данок на личен доход"
                , td (showPercentage procentiDanoci.dld10)
                , td (showInt model.danociDld10)
                , td "МКД"
                ]
            , R.tr_
                [ tdLeft "Вкупно придонеси и данок"
                , td ""
                , td (showInt model.vkupnoDavacki)
                , td "МКД"
                ]
            , R.tr
                { style: bold
                , children:
                    [ tdLeft "Нето"
                    , td ""
                    , td (showInt model.neto)
                    , td "МКД"
                    ]
                }
            ]
        ]
    }

mkMainView :: Component {}
mkMainView = do
  reducer <- reducerEff
  inputFields <- mkInputFields
  component "MainView" \_ -> React.do
    model /\ dispatch <- React.useReducer (bruto2neto 0.0) reducer
    pure
      $ R.div_
          [ R.div_ [ ribbon ]
          , R.div_ [ pdfLink ]
          , R.div
              { style: containerStyle
              , children:
                  [ inputFields { model: model, dispatch: dispatch }
                  , details model
                  ]
              }
          ]
