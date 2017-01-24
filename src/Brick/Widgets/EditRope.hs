{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides a basic text editor widget. You'll need to
-- embed an 'Editor' in your application state and transform it with
-- 'handleEvent' when relevant events arrive. To get the contents
-- of the editor, just use 'getEditContents'. To modify it, use the
-- 'Z.TextZipper' interface with 'applyEdit'.
--
-- The editor's 'HandleEvent' instance handles a set of basic input
-- events that should suffice for most purposes; see the source for a
-- complete list.
module Brick.Widgets.EditRope
  ( Editor(editContents, editorName, editDrawContents)
  -- * Constructing an editor
  , editor
  -- * Reading editor contents
  , getEditContents
  -- * Handling events
  , handleEditorEvent
  -- * Editing text
  , applyEdit
  -- * Lenses for working with editors
  , editContentsL
  , editDrawContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , editFocusedAttr
  )
where

import Data.Monoid
import Lens.Micro
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import qualified Yi.Rope as Y
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap

-- | Editor state.  Editors support the following events by default:
--
-- * Ctrl-a: go to beginning of line
-- * Ctrl-e: go to end of line
-- * Ctrl-d, Del: delete character at cursor position
-- * Backspace: delete character prior to cursor position
-- * Ctrl-k: delete all from cursor to end of line
-- * Ctrl-u: delete all from cursor to beginning of line
-- * Arrow keys: move cursor
-- * Enter: break the current line at the cursor position
data Editor n =
    Editor { editContents :: Y.YiString
           -- ^ The contents of the editor
           , editDrawContents :: Y.YiString -> Widget n
           -- ^ The function the editor uses to draw its contents
           , editorName :: n
           -- ^ The name of the editor
           , cursorPos :: (Int, Int)
           }

suffixLenses ''Editor

instance TextWidth Y.YiString where
  textWidth = V.wcswidth . Y.toString
  
instance (Show n) => Show (Editor n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show (editContents e)
               , ", editorName = " <> show (editorName e)
               , "}"
               ]

instance Named (Editor n) n where
    getName = editorName

handleEditorEvent :: Event -> Editor n -> EventM n (Editor n)
handleEditorEvent e ed =
        let move d = \(x, y) -> (x + d, y)
            (f, m) = case e of
                  -- EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                  -- EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                  -- EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                  -- EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                  -- EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
                  -- EvKey KEnter [] -> Z.breakLine
                  -- EvKey KDel [] -> Z.deleteChar
                  EvKey (KChar c) [] | c /= '\t' -> (Y.cons c, id) -- TODO insert at cursorPosL
                  -- EvKey KUp [] -> Z.moveUp
                  -- EvKey KDown [] -> Z.moveDown
                  EvKey KLeft [] -> (id, move (-1)) -- Z.moveLeft
                  EvKey KRight [] -> (id, move 1) -- Z.moveRight
                  -- EvKey KBS [] -> Z.deletePrevChar
                  _ -> (id, id)
        in return $ applyEdit f m ed

-- | Construct an editor over 'String' values
editor ::
       n
       -- ^ The editor's name (must be unique)
       -> (Y.YiString -> Widget n)
       -- ^ The content rendering function
       -> Y.YiString
       -- ^ The initial content
       -> Editor n
editor name draw s = Editor s draw name (0, 0)

-- | Apply an editing operation to the editor's contents. Bear in mind
-- that you should only apply operations that operate on the
-- current line; the editor will only ever render the first line of
-- text.
applyEdit :: (Y.YiString -> Y.YiString)
          -- ^ The editing transformation to apply
          -> ((Int, Int) -> (Int, Int))
          -> Editor n
          -> Editor n
applyEdit f m e = (e & cursorPosL %~ m) & editContentsL %~ f

-- | The attribute assigned to the editor when it does not have focus.
editAttr :: AttrName
editAttr = "edit"

-- | The attribute assigned to the editor when it has focus. Extends
-- 'editAttr'.
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> "focused"

-- | Get the contents of the editor.
getEditContents :: Editor n -> Y.YiString
getEditContents e = e^.editContentsL

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor :: (Ord n, Show n)
             => Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor n
             -- ^ The editor.
             -> Widget n
renderEditor foc e =
    let z = e^.editContentsL
        cp = e^.cursorPosL
        
        -- TODO toLeft = Z.take (cp^._2) (Z.currentLine z)
        toLeft = Y.fromString "edit1"
        
        -- TODO cursorLoc = Location (textWidth toLeft, cp^._1)
        cursorLoc = Location cp
        
        atChar = charAtCursor $ e^.editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       viewport (e^.editorNameL) Both $
       clickable (e^.editorNameL) $
       (if foc then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       e^.editDrawContentsL $ getEditContents e

charAtCursor :: Y.YiString -> Maybe Y.YiString
charAtCursor z =
    Just "e"
    -- TODO
    -- let col = snd $ Z.cursorPosition z
    --     curLine = Z.currentLine z
    --     toRight = Z.drop col curLine
    -- in if Z.length toRight > 0
    --    then Just $ Z.take 1 toRight
    --    else Nothing
