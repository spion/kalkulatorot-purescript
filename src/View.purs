module View
  ( bold
  , containerStyle
  , pdfLinkStyle
  , pdfLinkTxt
  , ribbon
  )
  where


import Prelude

import Danok (Model, bruto2neto, licnoOsloboduvanje, neto2bruto, procentiDanoci, procentiPridonesi)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (round)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, JSX, Reducer, component, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React

data Action = Bruto String | Neto String

reducerFn :: Model -> Action -> Model
reducerFn model msg = case msg of
    Bruto b ->
      case fromString b of
        Nothing -> model
        Just bv -> bruto2neto bv
    Neto n ->
      case fromString n of
        Nothing -> model
        Just nv -> neto2bruto nv

reducerEff :: Effect (Reducer Model Action)
reducerEff = mkReducer reducerFn



-- containerStyle : List (Attribute msg)
containerStyle :: R.CSS
containerStyle = R.css {
  width: "600px",
  left: "50%",
  marginLeft: "-300px",
  position: "absolute",
  verticalAlign: "center",
  font: "0.8em sans-serif"
}

    -- [ width: "600px"
    -- , left: "50%"
    -- , margin-left: "-300px"
    -- , position: "absolute"
    -- , vertical-align: "center"
    -- , font: "0.8em sans-serif"
    -- ]


bold :: R.CSS
bold = R.css { fontWeight: "bold" }



ribbon :: JSX
ribbon =
    R.a { href: "https://github.com/skopjehacklab/kalkulator.ot.mk",
        children: [ R.img { style: R.css {
                position: "absolute"
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
pdfLinkTxt =
    "Закон за данокот на личен доход"


pdfLinkStyle :: R.CSS
pdfLinkStyle = R.css
  { float: "right"
  , padding: "10px"
  , fontSize: "12px"
  , backgroundColor: "#fffcda"
  , color: "#000000"
  }


pdfLink :: JSX
pdfLink =
    R.a { style: pdfLinkStyle,
          href: "http://ujp.gov.mk/e/regulativa/opis/337",
          title: pdfLinkTxt,
          target: "_blank",
          rel: "noopener",
          children: [ R.text pdfLinkTxt ]
        }




splitter :: R.CSS
splitter = R.css
    { marginBottom: "30px"
    , borderBottom: "5px solid #afafaf"
    , borderLeft: "5px solid #afafaf"
    , borderRight: "5px solid #afafaf"
    , padding: "30px 25px"
    , backgroundColor: "#fffcda"
    , width: "600px"
    }


inputStyle :: R.CSS
inputStyle = R.css
    { boxSizing: "border-box"
    , lineHeight: "1.25"
    , padding: ".5rem .75rem"
    , backgroundClip: "padding-box"
    , width: "250px"
    , borderRadius: ".25rem"
    , border: "1px solid rgba(0,0,0,.15)"
    }


rowStyle :: R.CSS
rowStyle = R.css
    { borderBottom: "1px solid #afafaf"
    , padding: "15px"
    }


td :: String -> JSX
td txt =
  R.td { style: R.css { textAlign: "right"}, children: [ R.text txt ] }


tdLeft :: String -> JSX
tdLeft txt =
    R.td {style: rowStyle, children: [ R.text txt ]}


inputFields :: Component {model :: Model, dispatch :: Action -> Effect Unit }
inputFields = do

  -- reducer <- reducerEff

  component "InputFields" \props -> React.do

    -- state /\ dispatch <- React.useReducer (bruto2neto 0) reducer

    -- Need to deal with onInput Bruto
    pure $ R.table {style: splitter, children:
        [ R.tr_
            [ R.th_ [ R.text "Бруто" ]
            , R.th_ [ R.text "Нето" ]
            ]
        , R.tr_
            [ R.td_ [
                R.input {
                  title: "Бруто, износ кој ја вклучува чистата плата што ја добива работникот (нето-плата) заедно со сите јавни давачки (даноци и придонеси), во бруто-платата се вклучени надоместоците кои ги добиваат вработените за храна и за превоз",
                  type: "number",
                  placeholder: "Бруто",
                  onChange: handler targetValue \v -> props.dispatch (Bruto $ fromMaybe "" v),
                  value: (show props.model.bruto), style: inputStyle
                }
              ]
            , R.td_ [
                R.input {
                  title: "Нето, чистата плата што ја добива работникот на својата трансакциска сметка",
                  type: "number",
                  placeholder: "Нето",
                  onChange: handler targetValue \v -> props.dispatch (Neto $ fromMaybe "" v),
                  value: (show props.model.neto), style: inputStyle
                }
              ]
            ]
        ]
    }


infoIcon :: JSX
infoIcon =
    R.span {
        style: R.css
        { display: "inline-block"
        , width: "16px"
        , height: "16px"
        , textAlign: "center"
        , borderRadius: "50%"
        , background: "#9898ea"
        , color: "#fff"
        , marginRight: "5px"
        , userSelect: "none"
        },
        children: [ R.i_ [ R.text "i" ] ]
    }

showPercentage :: Number -> String
showPercentage num = (show $ round (num * 10000.0) / 100.0) <> "%"


details :: Model -> JSX
details model =
    R.div
      { style: R.css { margin: "0 0 50px 0" }
      , children:

        [ R.table_
            [ R.tr {style: bold, children:
                [ tdLeft "Бруто"
                , R.td_ [ R.text "" ]
                , R.td_ [ R.text (show model.bruto) ]
                , R.td_ [ R.text "МКД" ]
                ]}
            , R.tr_
                [ tdLeft "Придонеси за задолжително ПИО"
                , R.td_ [R.text $ showPercentage procentiPridonesi.penzisko]
                , R.td_ [R.text $ show model.pridonesi.penzisko]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Придонеси за задолжително здравствено осигурување"
                , R.td_ [R.text $ showPercentage procentiPridonesi.zdravstveno]
                , R.td_ [R.text $ show model.pridonesi.zdravstveno]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Придонес за осигурување во случај на невработеност"
                , R.td_ [R.text $ showPercentage procentiPridonesi.nevrabotenost]
                , R.td_ [R.text $ show model.pridonesi.nevrabotenost]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Дополнителен придонес за задолжително осигурување во случај повреда или професионално заболување"
                , R.td_ [R.text $ showPercentage procentiPridonesi.boluvanje]
                , R.td_ [R.text $ show model.pridonesi.boluvanje]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr {style: bold, children:
                [ tdLeft "Вкупно придонеси"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show model.vkupnoPridonesi]
                , R.td_ [R.text $ "МКД" ]
                ]}
            , R.tr_
                [ tdLeft "Бруто плата намалена за придонеси"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show model.brutoMinusPridonesi]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Лично ослободување"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show licnoOsloboduvanje]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Даночна основа за пресметка на данок на личен доход"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show model.dldOsnova10]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Данок на личен доход"
                , R.td_ [R.text $ showPercentage procentiDanoci.dld10]
                , R.td_ [R.text $ show model.danoci.dld10]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr_
                [ tdLeft "Вкупно придонеси и данок"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show model.vkupnoDavacki]
                , R.td_ [R.text $ "МКД" ]
                ]
            , R.tr {style: bold, children:
                [ tdLeft "Нето"
                , R.td_ [R.text ""]
                , R.td_ [R.text $ show model.neto]
                , R.td_ [R.text $ "МКД" ]
                ]}
            ]
        ]
      }


-- view : Model -> Html Msg
-- view model =
--     div []
--         [ div [] [ ribbon ]
--         , div [] [ pdfLink ]
--         , div containerStyle
--             [ inputFields model
--             , details model
--             ]
--         ]