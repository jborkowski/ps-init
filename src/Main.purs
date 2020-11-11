module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


data Action = Increment | Decrement

type Input = Unit
type State = Int

component :: forall quert input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
    initialState :: forall input. input -> State
    initialState _ = 0

    render :: forall m. State -> H.ComponentHTML Action () m
    render state =
      HH.div_
        [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
        , HH.text (show state)
        , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
        ]

    handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
      Decrement ->
        H.modify_ \state -> state - 1

      Increment ->
        H.modify_ \state -> state + 1


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
