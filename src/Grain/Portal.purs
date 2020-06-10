module Grain.Portal
  ( Config
  , portal
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Grain (class Grain, UI, VNode, fromConstructor, mountUI, patchUI, useLocalState)
import Grain.Markup as H
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, removeChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

-- | The type of portal config.
-- |
-- | - `rootZ`: `z-index` of portal root.
-- | - `child`: A `VNode` in portal root.
type Config =
  { rootZ :: Int
  , child :: VNode
  }

data Portal = Portal

instance showPortal :: Show Portal where
  show _ = "Portal"

newtype PortalState = PortalState
  { node :: Node
  , ui :: UI
  }

instance grainPortal :: Grain Portal (Maybe PortalState) where
  initialState _ = pure Nothing
  typeRefOf _ = fromConstructor Portal

-- | Render a `VNode` to portal root.
-- |
-- | The portal root will be created automatically.
portal :: Config -> VNode
portal config = H.component do
  Tuple maybePortal updatePortal <- useLocalState Portal

  let didCreate = do
        node <- createPortalRoot config.rootZ
        ui <- mountUI config.child node
        updatePortal $ const $ Just $ PortalState { node, ui }

      didUpdate =
        case maybePortal of
          Nothing -> pure unit
          Just (PortalState { ui }) ->
            patchUI (Just config.child) ui

      didDelete = do
        case maybePortal of
          Nothing -> pure unit
          Just (PortalState { node, ui }) -> do
            patchUI Nothing ui
            removePortalRoot node
            updatePortal $ const Nothing

  pure $ H.span
    # H.didCreate (const didCreate)
    # H.didUpdate (const didUpdate)
    # H.didDelete (const didDelete)

createPortalRoot :: Int -> Effect Node
createPortalRoot rootZ = do
  el <- window >>= document <#> toDocument >>= createElement "div"
  E.setAttribute "style" (getPortalStyle rootZ) el
  getBody >>= appendChild (E.toNode el)

removePortalRoot :: Node -> Effect Unit
removePortalRoot node =
  void $ getBody >>= removeChild node

getPortalStyle :: Int -> String
getPortalStyle rootZ = "position: absolute; z-index: " <> show rootZ <> ";"

getBody :: Effect Node
getBody =
  unsafePartial $ fromJust <$> (window >>= document >>= body) <#> HE.toNode
