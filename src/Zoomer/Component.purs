module Zoomer.Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except as Except
import DOM (DOM)
import DOM.Event.Event as Event
import DOM.Event.EventTarget as ETarget
import DOM.Event.Types as EType
import DOM.File.FileList as FileList
import DOM.File.FileReader.Aff as FileReader
import DOM.File.Types as FileTypes
import DOM.HTML.HTMLInputElement as HTMLInputElement
import DOM.HTML.Types as HTypes
import Data.Either as Either
import Data.Foreign as F
import Data.Foreign.Class (readEitherR)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Debug.Trace (traceAnyA)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Zoomer.InitialImage (initialImage)
import Halogen.Aff as HA

type State =
  { originalImage :: String
  , zoomRectangle :: C.Rectangle
  , steps :: Int
  }

data Query a
  = Initialize a
  | StartDragging EType.Event a
  | StopDragging EType.Event a
  | SetImage EType.Event a
  | SetSteps String a

type Effects eff = HA.HalogenEffects ()

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
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
    , zoomRectangle: { x: 10.0, y: 380.0, w: 64.0, h: 64.0 }
    , steps: 4
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
     []
     [ HH.input
         [ HP.type_ HP.InputFile
         , HE.onChange (HE.input SetImage)
         ]
     , HH.input
         [ HP.type_ HP.InputNumber
         , HE.onValueChange (HE.input SetSteps)
         ]
     , HH.img
         [ HP.src state.originalImage
         ]
     ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
  eval = case _ of
    Initialize next -> pure next
    StartDragging _ next -> pure next
    StopDragging _ next -> pure next
    SetImage event next -> do
      maybeFile <- H.liftAff $ case Except.runExcept $ HTypes.readHTMLInputElement $ F.toForeign $ Event.target event of
        Either.Left _ -> pure Nothing
        Either.Right inputElement -> do
          Nullable.toMaybe <$> H.liftEff (HTMLInputElement.files inputElement)
            >>= case _ of
                  Nothing -> pure Nothing
                  Just fileList ->
                    case Nullable.toMaybe $ FileList.item 0 fileList of
                      Nothing -> pure Nothing
                      Just file ->
                        Just <$> (FileReader.readAsDataURL $ FileTypes.fileToBlob file) 
      case maybeFile of
        Nothing -> pure unit
        Just file -> H.modify (_ { originalImage = file })
      pure next
    SetSteps _ next -> pure next
