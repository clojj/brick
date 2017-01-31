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
  , TokenizedEvent(..)
  -- , startGhc
  , startEvent
  )
where

import Data.Monoid
import Lens.Micro
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import System.IO
import Data.List
import Data.Maybe
import qualified Yi.Rope as Y
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap
import Brick.BChan

import GHC

import Control.Monad.IO.Class
import Control.Concurrent



data TokenizedEvent a = Tokens a
  deriving Show

type Loc = (Int, Int)

data Operation =
  InsertChar Char Loc
  | DeleteChar Loc
  | MoveCursor Loc
  | Undo
  | HandleTokens [Located Token]

instance Show Operation where
    show op = case op of
      InsertChar ch position -> "InsertChar " ++ [ch] ++ " " ++ show position
      DeleteChar position -> "DeleteChar " ++ show position
      MoveCursor d -> "MoveCursor " ++ show d
      Undo -> "Undo"
      HandleTokens tokens -> "HandleTokens"


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
           , editEventChannel :: BChan (TokenizedEvent [Located Token])
           , editLexerChannel :: MVar String
           -- TODO undo will be inverse of operation, depending on increment/decrement of index into this list
           , editOperations :: [Operation]
           -- TODO render tokens
           , editTokens :: [Located Token]
           , editSendSource :: String -> IO ()
           }
suffixLenses ''Editor

-- | Construct an editor over 'String' values
editor ::
       n
       -- ^ The editor's name (must be unique)
       -> (Y.YiString -> Widget n)
       -- ^ The content rendering function
       -> Y.YiString
       -- ^ The initial content
       -> BChan (TokenizedEvent [Located Token])
       -> MVar String
       -> (String -> IO ())
       -> Editor n
editor name draw s eventChannel lexerChannel sendSource = Editor s draw name (0, 0) eventChannel lexerChannel [] [] sendSource

-- TODO orphane instance !
-- instance TextWidth Y.YiString where
--   textWidth = V.wcswidth . Y.toString

instance (Show n) => Show (Editor n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show ((Y.toString.editContents) e)
               , ", editorName = " <> show (editorName e)
               , ", cursorPos = " <> show (editCursor e)
               , ", operations = " <> show (editOperations e)
              -- TODO , ", tokens = " <> show (editTokens e)
               , "}"
               ]

instance Named (Editor n) n where
    getName = editorName


-- TODO
handleTokens :: [Located Token] -> (Editor n -> Editor n)
handleTokens tokens = editTokensL .~ tokens

moveCursor :: Loc -> (Editor n -> Editor n)
moveCursor (column, line) =
  (editCursorL %~ moveColumn column) . (editCursorL %~ moveLine line)
  where
    moveColumn :: Int -> (Int, Int) -> (Int, Int)
    moveColumn newColumn (_, l) = (newColumn, l)
    moveLine :: Int -> (Int, Int) -> (Int, Int)
    moveLine newLine (c, _) = (c, newLine)

-- TODO refactor insertCh + deleteCh
insertCh :: Char -> Loc -> (Editor n -> Editor n)
insertCh ch position = editContentsL %~ insertChar ch position
  where
    insertChar :: Char -> Loc -> (Y.YiString -> Y.YiString)
    insertChar char (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
      in lBefore <> (cBefore <> Y.cons char cAfter)

deleteCh :: Loc -> (Editor n -> Editor n)
deleteCh position = editContentsL %~ deleteChar position
  where
    deleteChar :: Loc -> (Y.YiString -> Y.YiString)
    deleteChar (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
          cTail = fromMaybe Y.empty $ Y.tail cAfter
      in lBefore <> (cBefore <> cTail)

handleEditorEvent :: BrickEvent n (TokenizedEvent [Located Token]) -> Editor n -> EventM n (Editor n)
handleEditorEvent e ed = do
        let cp@(column, line) = ed ^. editCursorL
            ops = case e of
                  -- EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                  -- EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                  -- EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                  -- EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                  -- EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
                  -- EvKey KBS [] -> Z.deletePrevChar
                  -- EvKey KDel [] ->

                  VtyEvent (EvKey (KChar 'z') [MCtrl]) -> [Undo]

                  VtyEvent (EvKey KBS []) -> [DeleteChar cp]
                  VtyEvent (EvKey KEnter []) -> [InsertChar '\n' cp, MoveCursor (0, line + 1)]
                  VtyEvent (EvKey (KChar c) []) | c /= '\t' -> [InsertChar c cp, MoveCursor (column + 1, line)]

                  VtyEvent (EvKey KUp [])    -> [MoveCursor (column, line - 1)]
                  VtyEvent (EvKey KDown [])  -> [MoveCursor (column, line + 1)]
                  VtyEvent (EvKey KLeft [])  -> [MoveCursor (column - 1, line)]
                  VtyEvent (EvKey KRight []) -> [MoveCursor (column + 1, line)]
                  AppEvent (Tokens tokens) -> [HandleTokens tokens]

                  _ -> []

            contentsOps = filter modifiesContents ops
            ed' = consOps contentsOps (applyComposed ops ed)

        case ops of
          -- TODO refactor.. only send changed contents to lexer
          (DeleteChar _ : _) -> sendToLexer ed'
          (InsertChar _ _ : _) -> sendToLexer ed'
          _ -> return ed'

        -- liftIO $ hPrint stderr (ed' ^. editOperationsL)
        -- return ed'

        where
          modifiesContents :: Operation -> Bool
          modifiesContents op =
            case op of
              InsertChar _ _ -> True
              DeleteChar _ -> True
              _ -> False

sendToLexer :: Editor n -> EventM n (Editor n)
sendToLexer ed = do
  -- TODO
  liftIO $ editSendSource ed $ (Y.toString . editContents) ed
  liftIO $ putMVar (editLexerChannel ed) ((Y.toString . editContents) ed)
  return ed
  
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
    InsertChar ch position -> e & insertCh ch position
    DeleteChar position -> e & deleteCh position
    MoveCursor d -> e & moveCursor d
    Undo -> e & id -- TODO
    HandleTokens tokens -> e & handleTokens tokens -- TODO

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
renderEditor focus e =
    let cp = e ^. editCursorL

        -- TODO limit viewport like EditDemo.hs
        
        -- toLeft = Z.take (cp^._2) (Z.currentLine z)
        -- cursorLoc = Location (textWidth toLeft, cp^._1)
        -- limit = case e^.editContentsL.to Z.getLineLimit of
        --     Nothing -> id
        --     Just lim -> vLimit lim

        cursorLoc = Location cp
        atChar = charAtCursor cp $ e ^. editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if focus then editFocusedAttr else editAttr) $
       -- TODO limit $
       viewport (e^.editorNameL) Both $
       clickable (e^.editorNameL) $
       (if focus then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       e^.editDrawContentsL $ getEditContents e

charAtCursor :: (Int, Int) -> Y.YiString -> Maybe String
charAtCursor (column, line) s =
  let toRight = snd $ Y.splitAt column (snd $ Y.splitAtLine line s)
  in fmap (replicate 1) (Y.head toRight)


startEvent :: Editor n -> EventM n (Editor n)
startEvent ed = do
  liftIO $ hPrint stderr $ "in startEvent: " ++ Y.toString (ed^.editContentsL)
  -- TODO remove: start lexer here is probably not viable... it should be shared among all editors for one stack-project
  -- lexerChannel <- liftIO newEmptyMVar
  -- let ed' = ed & (editLexerChannelL .~ Just lexerChannel)
  -- liftIO $ startGhc (ed' ^. editEventChannelL) lexerChannel
  return ed
