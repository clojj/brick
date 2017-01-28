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
  -- , applyEdit
  , applyComposed
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

import System.IO
import Data.List
import Data.Maybe
import qualified Yi.Rope as Y
import Control.Monad.IO.Class
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap


type Loc = (Int, Int)

data Operation =
  InsertChar Char Loc
  | DeleteChar Loc
  | MoveCursor Loc
  | Undo

instance Show Operation where
    show op = case op of
      InsertChar ch loc -> "InsertChar " ++ [ch] ++ " " ++ show loc
      DeleteChar loc -> "DeleteChar " ++ show loc
      MoveCursor d -> "MoveCursor " ++ show d
      Undo -> "Undo"
  

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
           , editCursor :: Loc
           , editOperations :: [Operation]
           }

suffixLenses ''Editor

-- TODO orphane instance !
instance TextWidth Y.YiString where
  textWidth = V.wcswidth . Y.toString

instance (Show n) => Show (Editor n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show ((Y.toString.editContents) e)
               , ", editorName = " <> show (editorName e)
               , ", cursorPos = " <> show (editCursor e)
               , ", operations = " <> show (editOperations e)
               , "}"
               ]

instance Named (Editor n) n where
    getName = editorName


moveCursor :: Loc -> (Editor n -> Editor n)
moveCursor (c, l) = 
  (editCursorL %~ moveColumn c) . (editCursorL %~ moveLine l)
  where
    moveColumn :: Int -> (Int, Int) -> (Int, Int)
    moveColumn cd (c, l) = (c + cd, l)
    moveLine :: Int -> (Int, Int) -> (Int, Int)
    moveLine ld (c, l) = (c, l + ld)

insertCh :: Char -> Loc -> (Editor n -> Editor n)
insertCh ch loc = editContentsL %~ insertChar ch loc
  where
    insertChar :: Char -> Loc -> (Y.YiString -> Y.YiString)
    insertChar char (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
      in lBefore <> (cBefore <> Y.cons char cAfter)

deleteCh :: Loc -> (Editor n -> Editor n)
deleteCh loc = editContentsL %~ deleteChar loc
  where
    deleteChar :: Loc -> (Y.YiString -> Y.YiString)
    deleteChar (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
          cTail = fromMaybe Y.empty $ Y.tail cAfter
      in lBefore <> (cBefore <> cTail)


handleEditorEvent :: Event -> Editor n -> EventM n (Editor n)
handleEditorEvent e ed = do
        let cp = ed ^. editCursorL
            ops = case e of
                  -- EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                  -- EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                  -- EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                  -- EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                  -- EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
                  -- EvKey KBS [] -> Z.deletePrevChar
                  -- EvKey KDel [] ->

                  EvKey (KChar 'z') [MCtrl] -> [Undo]

                  EvKey KBS [] -> [DeleteChar cp]
                  EvKey KEnter [] -> [InsertChar '\n' cp, MoveCursor (1, 0)]
                  EvKey (KChar c) [] | c /= '\t' -> [InsertChar c cp, MoveCursor (1, 0)]

                  EvKey KUp []    -> [MoveCursor (0, -1)]
                  EvKey KDown []  -> [MoveCursor (0, 1)]
                  EvKey KLeft []  -> [MoveCursor (-1, 0)]
                  EvKey KRight [] -> [MoveCursor (1, 0)]
                  _ -> []
                  
            contentsOps = filter modifiesContents ops
        let ed' = consOps contentsOps (applyComposed ops ed)
        liftIO $ hPrint stderr (ed' ^. editOperationsL)
        return ed'
        where
          modifiesContents :: Operation -> Bool
          modifiesContents op = 
            case op of
              InsertChar _ _ -> True
              DeleteChar _ -> True
              _ -> False

consOps :: [Operation] -> Editor n -> Editor n
consOps ops e = 
  case ops of
    [] -> e
    (op : _) -> e & editOperationsL %~ (\l -> op : l)

applyComposed :: [Operation] -> Editor n -> Editor n
applyComposed fs ed = foldl' foldOperation ed fs

foldOperation :: Editor n -> Operation -> Editor n
foldOperation e op =
  case op of
    InsertChar ch loc -> e & insertCh ch loc
    DeleteChar loc -> e & deleteCh loc
    MoveCursor d -> e & moveCursor d
    Undo -> e & id -- TODO

-- | Construct an editor over 'String' values
editor ::
       n
       -- ^ The editor's name (must be unique)
       -> (Y.YiString -> Widget n)
       -- ^ The content rendering function
       -> Y.YiString
       -- ^ The initial content
       -> Editor n
editor name draw s = Editor s draw name (0, 0) []

-- | Apply an editing operation to the editor's contents. Bear in mind
-- that you should only apply operations that operate on the
-- current line; the editor will only ever render the first line of
-- text.
-- applyEdit ::
--           -- ^ The editing transformation to apply
--           (Y.YiString -> Y.YiString)
--           -> Editor n
--           -> Editor n
-- applyEdit f e = e & editContentsL %~ f

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
    let cp = e ^. editCursorL
        cursorLoc = Location cp
        atChar = charAtCursor cp $ e ^. editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       viewport (e^.editorNameL) Both $
       clickable (e^.editorNameL) $
       (if foc then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       e^.editDrawContentsL $ getEditContents e

charAtCursor :: (Int, Int) -> Y.YiString -> Maybe Y.YiString
charAtCursor (c, l) s =
    Just "X" -- TODO what is the role of textWidth ? for variable-width fonts ?

    -- let col = snd $ Z.cursorPosition z
    --     curLine = Z.currentLine z
    --     toRight = Z.drop col curLine
    -- in if Z.length toRight > 0
    --    then Just $ Z.take 1 toRight
    --    else Nothing
