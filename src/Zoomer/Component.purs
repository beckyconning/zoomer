module Zoomer.Component where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Graphics.Canvas as C
import DOM.Event.Types as ET
import Zoomer.InitialImage (initialImage)

type State =
  { originalImage :: String
  , zoomRectangle :: C.Rectangle
  }

data Query a
  = Initialize a
  | StartDragging ET.Event a
  | StopDragging ET.Event a
  | SetImage ET.Event a

data Message = Toggled Boolean

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState =
    { originalImage: initialImage
    , zoomRectangle: { x: 10, y: 380, w: 64, h: 64 }
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [] []

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Initialize next -> pure next
    StartDragging _ next -> pure next
    StopDragging _ next -> pure next
    SetImage _ next -> pure next
