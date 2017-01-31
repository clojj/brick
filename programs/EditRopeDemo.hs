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
  , startEventLensed
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
import Brick.BChan

import qualified Yi.Rope as Y

import ErrUtils (mkPlainErrMsg)
import FastString (mkFastString)
import GHC
import GHC.Paths (libdir)
import Lexer
import qualified MonadUtils as GMU
import SrcLoc
import StringBuffer

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import System.IO
import qualified Control.FoldDebounce as Fdeb


data EditorName = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing EditorName
       , _edit1 :: E.Editor EditorName
       , _edit2 :: E.Editor EditorName
       }

makeLenses ''St

drawUI :: St -> [Widget EditorName]
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

appEvent :: St -> BrickEvent EditorName (E.TokenizedEvent [GHC.Located GHC.Token]) -> EventM EditorName (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> M.halt st
        VtyEvent (V.EvKey (V.KChar '\t') []) -> M.continue $ st & focusRing %~ F.focusNext
        VtyEvent (V.EvKey V.KBackTab []) -> M.continue $ st & focusRing %~ F.focusPrev

        appEvt@(AppEvent (E.Tokens _)) -> handleInEditor st appEvt

        vtyEv@(VtyEvent (V.EvKey _ _)) -> handleInEditor st vtyEv

        _ -> M.continue st


handleInEditor :: St -> BrickEvent EditorName (E.TokenizedEvent [GHC.Located GHC.Token]) -> EventM EditorName (Next St)
handleInEditor st e =
  M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
       Just Edit1 -> handleEventLensed st edit1 E.handleEditorEvent e
       Just Edit2 -> handleEventLensed st edit2 E.handleEditorEvent e
       Nothing -> return st

initialState :: BChan (E.TokenizedEvent [GHC.Located GHC.Token]) -> MVar String -> (String -> IO ()) -> St
initialState eventChannel lexerChannel sendSource =
    St (F.focusRing [Edit1, Edit2])
       -- TODO build yiStr function for rendering Y.YiString
       (E.editor Edit1 (str . Y.toString) "edit1" eventChannel lexerChannel sendSource)
       (E.editor Edit2 (str . Y.toString) "edit2" eventChannel lexerChannel sendSource)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [CursorLocation EditorName] -> Maybe (CursorLocation EditorName)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St (E.TokenizedEvent [GHC.Located GHC.Token]) EditorName
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = startEvent -- return
          , M.appAttrMap = const theMap
          }

startEvent :: St -> EventM EditorName St
startEvent st = do
  startEventLensed st edit1 E.startEvent
  startEventLensed st edit2 E.startEvent

startGhc :: BChan (E.TokenizedEvent [Located Token]) -> MVar String -> IO ()
startGhc eventChannel lexerChannel = do
  threadId <- forkIO $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
    GMU.liftIO $ forever $ do
        src <- takeMVar lexerChannel
        let sb = stringToStringBuffer src
        let pResult = lexTokenStream sb lexLoc flags
        case pResult of

          POk _ toks -> GMU.liftIO $ do
            hPrint stderr $ "TOKENS\n" ++ concatMap showToken toks
            writeBChan eventChannel $ E.Tokens toks

          PFailed srcspan msg -> do
            GMU.liftIO $ print $ show srcspan
            GMU.liftIO $
              do putStrLn "Lexer Error:"
                 print $ mkPlainErrMsg flags srcspan msg
  return ()

showToken :: GenLocated SrcSpan Token -> String
showToken t = "\nsrcLoc: " ++ srcloc ++ "\ntok: " ++ tok ++ "\n"
  where
    srcloc = show $ getLoc t
    tok = show $ unLoc t

showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++
  tok ++ "\n" ++ "Source: " ++ src ++ "\n" ++ "Location: " ++ srcloc ++ "\n\n"
  where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok


main :: IO ()
main = do
  eventChannel <- newBChan 1
  lexerChannel <- liftIO newEmptyMVar

  liftIO $ startGhc eventChannel lexerChannel

  -- TODO replace (hPrint stderr) with the real thing
  trigger <- Fdeb.new Fdeb.Args { Fdeb.cb = hPrint stderr, Fdeb.fold = \_ i -> i, Fdeb.init = "" }
                      Fdeb.def { Fdeb.delay = 300000 }
  let sendSource = Fdeb.send trigger

  let initSt = initialState eventChannel lexerChannel sendSource
  st <- M.customMain (V.mkVty V.defaultConfig) (Just eventChannel) theApp initSt

  putStrLn "In input 1 you entered:\n"
  putStrLn $ Y.toString $ E.getEditContents $ st^.edit1
  putStrLn "In input 2 you entered:\n"
  putStrLn $ Y.toString $ E.getEditContents $ st^.edit2
