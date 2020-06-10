module Main where

import Prelude

import Data.Array (foldl, (..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object, empty, insert, update, values)
import Grain (class Grain, VNode, fromConstructor, mountUI, useLocalState)
import Grain.Markup as H
import Grain.Portal (portal)
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (stopPropagation)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

data Items = Items

newtype ItemMap = ItemMap (Object Item)

derive instance newtypeItemMap :: Newtype ItemMap _

type Item =
  { id :: String
  , opened :: Boolean
  }

instance showItems :: Show Items where
  show _ = "Items"

instance grainItems :: Grain Items ItemMap where
  typeRefOf _ = fromConstructor Items
  initialState _ = pure $ ItemMap $ foldl genItem empty (0 .. 3)
    where
      genItem obj i =
        insert (show i) { id: show i, opened: false } obj

open :: String -> ItemMap -> ItemMap
open id = wrap <<< (update (Just <<< _ { opened = true }) id) <<< unwrap

close :: String -> ItemMap -> ItemMap
close id = wrap <<< update (Just <<< _ { opened = false }) id <<< unwrap

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#app")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      void $ mountUI view $ toNode el

view :: VNode
view = H.component do
  Tuple (ItemMap items) updateItems <- useLocalState Items
  let openItem item = updateItems $ open item.id
      closeItem item = updateItems $ close item.id
  pure $ H.div # H.kids
    [ H.h1 # H.kids [ H.text "Portal Demo" ]
    , H.ul # H.kids
        ( values items <#> \item ->
            itemView
              { item
              , onOpen: openItem item
              , onClose: closeItem item
              }
        )
    ]

type ItemViewProps =
  { item :: Item
  , onOpen :: Effect Unit
  , onClose :: Effect Unit
  }

itemView :: ItemViewProps -> VNode
itemView { item, onOpen, onClose } =
  H.li
    # H.key (item.id)
    # H.css style
    # H.onClick (const onOpen)
    # H.kids
        [ H.span # H.kids [ H.text $ "Item " <> item.id ]
        , dialog { item, onClose }
        ]
  where
    style =
      """
      .& { cursor: pointer; }
      .&:hover { opacity: 0.5; }
      """

type DialogProps =
  { item :: Item
  , onClose :: Effect Unit
  }

dialog :: DialogProps -> VNode
dialog { item, onClose } =
  if not item.opened
    then H.div
    else
      portal
        { rootZ: 0
        , child:
            H.div
              # H.css overlayStyle
              # H.onClick (const onClose)
              # H.kids
                  [ H.div # H.css boxStyle # H.onClick stopPropagation # H.kids
                      [ H.text $ "Dialog: Item " <> item.id
                      ]
                  ]
        }
  where
    overlayStyle =
      """
      .& {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0,0,0,.1);
        display: flex;
        justify-content: center;
        align-items: center;
        animation-fill-mode: both;
        animation: & 0.2s ease-in;
      }
      @keyframes & {
        from { opacity: 0 }
        to { opacity: 1 }
      }
      """
    boxStyle =
      """
      .& {
        width: 60%;
        height: 80%;
        background-color: white;
        border: 1px solid #DDD;
        border-radius: 8px;
        display: flex;
        justify-content: center;
        align-items: center;
        font-weight: bold;
        font-size: 24px;
      }
      """
