{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
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


data Name = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor Name
       , _edit2 :: E.Editor Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit2)

        ui = C.center $
            (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 $ vLimit 5 e2)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

initialState :: (MVar String, MVar [Located Token]) -> St
initialState channels =
    St (F.focusRing [Edit1, Edit2])
       -- TODO build yiStr function for rendering Y.YiString
       (E.editor Edit1 (str . Y.toString) "edit1" channels)
       (E.editor Edit2 (str . Y.toString) "edit2" channels)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    channels <- startGhc
    st <- M.defaultMain theApp $ initialState channels
    putStrLn "In input 1 you entered:\n"
    putStrLn $ Y.toString $ E.getEditContents $ st^.edit1
    putStrLn "In input 2 you entered:\n"
    putStrLn $ Y.toString $ E.getEditContents $ st^.edit2


startGhc :: IO (MVar String, MVar [Located Token])
startGhc = do
  chIn <- newEmptyMVar
  chOut <- newEmptyMVar
  threadId <- forkIO $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
    GMU.liftIO $ forever $ do
        str <- takeMVar chIn
        let sb = stringToStringBuffer str
        let pResult = lexTokenStream sb lexLoc flags
        case pResult of

          POk _ toks -> GMU.liftIO $ do
            hPrint stderr $ "TOKENS\n" ++ concatMap showToken toks
            -- TODO putMVar chOut toks

          PFailed srcspan msg -> do
            GMU.liftIO $ print $ show srcspan
            GMU.liftIO $
              do putStrLn "Lexer Error:"
                 print $ mkPlainErrMsg flags srcspan msg
  return $ (chIn, chOut)


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
