module Main where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as AXRF
import Control.Category (identity)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (input, label, output, slot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import PSCI.Support (eval)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

-- | First part - Without effect
{-
data Action = Increment | Decrement

type Input = Unit
type State = Int

component :: forall query input output m. H.Component HH.HTML query input output m
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
-}

-- | With Effect - Random
{-
type State = Maybe Number

data Action = Regenerate

component :: forall query input output m. MonadEffect m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let value = maybe "No number generated yet" show state
  HH.div_
    [ HH.h1_
      [ HH.text "Random numer" ]
    , HH.p_
      [ HH.text ("Current value: " <> value) ]
    , HH.button
      [ HE.onClick \_ -> Just Regenerate ]
      [ HH.text "Generate new number" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random -- lift Effect to MonadEffect
    H.modify_ \_ -> Just newNumber
-}

-- type State =
--   { loading :: Boolean
--   , username :: String
--   , result :: Maybe String
--   }

-- data Action =
--     SetUsername String
--   | MakeRequest Event

-- component :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
-- component =
--   H.mkComponent
--     { initialState
--     , render
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--     }

-- initialState :: forall i. i -> State
-- initialState _ = { loading: false, username: "", result: Nothing }

-- render :: forall m. State -> H.ComponentHTML Action () m
-- render st =
--   HH.form
--     [ HE.onSubmit \ev -> Just (MakeRequest ev) ]
--     [ HH.h1_ [ HH.text "Look up GitHub user" ]
--     , HH.label_
--         [ HH.div_ [ HH.text "Enter username:" ]
--         , HH.input
--             [ HP.value st.username
--             , HE.onValueInput \str -> Just (SetUsername str)
--             ]
--         ]
--     , HH.button
--         [ HP.disabled st.loading
--         , HP.type_ HP.ButtonSubmit
--         ]
--         [ HH.text "Fetch info" ]
--     , HH.p_
--         [ HH.text (if st.loading then "Working..." else "") ]
--     , HH.div_
--         case st.result of
--           Nothing -> []
--           Just res ->
--             [ HH.h2_
--                 [ HH.text "Response:" ]
--             , HH.pre_
--                 [ HH.code_ [ HH.text res ] ]
--             ]
--     ]

-- handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
-- handleAction = case _ of
--   SetUsername username -> do
--     H.modify_ _ { username = username, result = Nothing }

--   MakeRequest event -> do
--     H.liftEffect $ Event.preventDefault event
--     username <- H.gets _.username
--     H.modify_ _ { loading = true }
--     response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
--     H.modify_ _ { loading = false, result = map _.body (hush response) }

type Slots = ( button :: forall query. H.Slot query Void Int )
_button = SProxy :: SProxy "button"

parent :: forall query input output m. H.Component HH.HTML query input output m
parent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
    render :: forall state action. state -> H.ComponentHTML action Slots m
    render _ =
      HH.div_ [ HH.slot _button 0 button { label: "Click Me" } absurd ]

-- button :: forall w i. { label :: String } -> HH.HTML w i
-- button { label } = HH.button [ ] [ HH.text label ]

type Input = { label :: String }
type State = { label :: String }

button :: forall query output m. H.Component HH.HTML query Input output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
    initialState :: Input -> State
    initialState input = input

    render :: forall action. State -> H.ComponentHTML action () m
    render { label } = HH.button [ ] [ HH.text label ]
