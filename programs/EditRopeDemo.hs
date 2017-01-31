{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
-- import qualified Brick.Types as T
import Brick.Types
  ( Widget
  , Next
  , EventM
  , BrickEvent(..)
  , CursorLocation
  , handleEventLensed
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.EditRope as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import qualified Yi.Rope as Y

import ErrUtils (mkPlainErrMsg)
import FastString (mkFastString)
import GHC hiding (Name(..))
import GHC.Paths (libdir)
import Lexer
import qualified MonadUtils as GMU
import SrcLoc
import StringBuffer

import Control.Monad
import Control.Concurrent
import System.IO

import Brick.BChan



data Name = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor Name
       , _edit2 :: E.Editor Name
       }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit2)

        ui = C.center $
            (str "Input 1 (unlimited): " <+> hLimit 30 (vLimit 5 e1)) <=>
            str " " <=>
            (str "Input 2 (limited to 2 lines): " <+> hLimit 30 (vLimit 5 e2)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> BrickEvent Name (E.TokenizedEvent [Located Token]) -> EventM Name (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> M.halt st
        VtyEvent (V.EvKey (V.KChar '\t') []) -> M.continue $ st & focusRing %~ F.focusNext
        VtyEvent (V.EvKey V.KBackTab []) -> M.continue $ st & focusRing %~ F.focusPrev

        appEvt@(AppEvent (E.Tokens _)) -> handleInEditor st appEvt

        vtyEv@(VtyEvent (V.EvKey _ _)) -> handleInEditor st vtyEv

appEvent st _ = M.continue st

handleInEditor :: St -> BrickEvent Name (E.TokenizedEvent [Located Token]) -> EventM Name (Next St)
handleInEditor st e =
  M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
       Just Edit1 -> handleEventLensed st edit1 E.handleEditorEvent e
       Just Edit2 -> handleEventLensed st edit2 E.handleEditorEvent e
       Nothing -> return st

initialState :: BChan (E.TokenizedEvent [Located Token]) -> MVar String -> St
initialState evtChannel lexerChannel =
    St (F.focusRing [Edit1, Edit2])
       -- TODO build yiStr function for rendering Y.YiString
       (E.editor Edit1 (str . Y.toString) "edit1" evtChannel lexerChannel)
       (E.editor Edit2 (str . Y.toString) "edit2" evtChannel lexerChannel)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St (E.TokenizedEvent [Located Token]) Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = startEvent -- return
          , M.appAttrMap = const theMap
          }

startEvent :: St -> EventM Name St
startEvent st = E.startEvent (st^.edit1) st

main :: IO ()
main = do
  evtChannel <- newBChan 1
  lexerChannel <- newEmptyMVar
  -- E.startGhc evtChannel lexerChannel
  
  st <- M.customMain (V.mkVty V.defaultConfig) (Just evtChannel) theApp (initialState evtChannel lexerChannel)
  
  putStrLn "In input 1 you entered:\n"
  putStrLn $ Y.toString $ E.getEditContents $ st^.edit1
  putStrLn "In input 2 you entered:\n"
  putStrLn $ Y.toString $ E.getEditContents $ st^.edit2
