module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource as EventSource
import Halogen.VDom.Driver (runUI)

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

type ParentState = { count :: Int }

data ParentAction = Initialize | Increment

parent :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
    initialState :: input -> ParentState
    initialState _ = { count: 0 }

    render :: ParentState -> H.ComponentHTML ParentAction Slots m
    render { count } =
      HH.div_ [ HH.slot _button 0 button { label: show count } absurd ]

    handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
    handleAction = case _ of
      Initialize -> do
        void $ H.subscribe $ EventSource.affEventSource \emitter -> do
          fiber <- Aff.forkAff $ forever do
            Aff.delay $ Milliseconds 1000.0
            EventSource.emit emitter Increment

          pure $ EventSource.Finalizer do
            Aff.killFiber (error " Event source finalized") fiber

      Increment ->
        H.modify_ \st -> st { count = st.count + 1 }


-- button :: forall w i. { label :: String } -> HH.HTML w i
-- button { label } = HH.button [ ] [ HH.text label ]

type ButtonInput = { label :: String }
type ButtonState = { label :: String }
data ButtonAction = Receive ButtonInput

button :: forall query output m. H.Component HH.HTML query ButtonInput output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where
    initialState :: ButtonInput -> ButtonState
    initialState { label } = { label }

    render :: ButtonState -> H.ComponentHTML ButtonAction () m
    render { label } = HH.button_ [ HH.text label ]

    handleAction :: ButtonAction -> H.HalogenM ButtonState ButtonAction () output m Unit
    handleAction = case _ of
      Receive input ->
        H.modify_ _ { label = input.label }
