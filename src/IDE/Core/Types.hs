{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.Types (
    IDE(..)
,   IDEState(..)
,   IDERef
,   IDEM
,   IDEEventM
,   IDEAction
,   IDEEvent(..)
,   MonadIDE
,   liftIDE
,   (?>>=)

,   WorkspaceM
,   WorkspaceAction
,   runWorkspace

,   PackageM
,   PackageAction
,   runPackageM

,   DebugM
,   DebugAction
,   runDebugM

,   IDEPackage(..)
,   ipdBuildDir
,   ipdAllDirs
,   Workspace(..)
,   wsAllPackages
,   VCSConf

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)
,   cabalCommand
,   EditorStyle(..)
,   editorStyle

,   LogRefType(..)
,   LogRef(..)
,   logRefRootPath
,   logRefFilePath
,   logRefFullFilePath
,   isError
,   isBreakpoint
,   displaySrcSpan
,   colorHexString

,   SearchHint(..)
,   CandyTable(..)
,   CandyTableForth
,   CandyTableBack
,   KeymapI(..)
,   SpecialKeyTable
,   SpecialKeyCons

,   PackageDescrCache
,   ModuleDescrCache

,   CompletionWindow(..)
,   LogLaunch(..)
,   LogLaunchData(..)
,   LogTag(..)
,   GUIHistory
,   GUIHistory'(..)
,   SensitivityMask(..)
,   SearchMode(..)
,   StatusbarCompartment(..)
) where

import qualified IDE.YiConfig as Yi
import Graphics.UI.Gtk
       (TextBuffer, MenuItem, Window(..), KeyVal(..), Color(..), Menu(..),
        TreeView(..), ListStore(..), Toolbar(..))
import Data.Unique (newUnique, Unique(..))
import Graphics.UI.Frame.Panes
import Distribution.Package
    (PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (BuildInfo)
import Data.Map (Map(..))
import Data.Set (Set(..))
import Data.List (nubBy)
import Control.Concurrent (MVar)
import Distribution.ModuleName (ModuleName(..))
import Graphics.UI.Gtk.Gdk.EventM (Modifier(..))
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager(MergeId)
import System.Time (ClockTime(..))
import Distribution.Simple (Extension(..))
import IDE.Utils.Tool (ToolState(..), ProcessHandle)
import Data.IORef (writeIORef, readIORef, IORef(..))
import Numeric (showHex)
import Control.Event
    (EventSelector(..), EventSource(..), Event(..))
import System.FilePath (dropFileName, (</>))
import IDE.Core.CTypes
import IDE.StrippedPrefs(RetrieveStrategy)
import System.IO (Handle)
import Distribution.Text(disp)
import Text.PrettyPrint (render)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Time (UTCTime(..))

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified Data.Map as Map (Map)
import Data.Typeable (Typeable)
import Foreign (Ptr)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Language.Haskell.HLint3 (Idea(..))
import Data.Function (on)
import Control.Concurrent.STM.TVar (TVar)
import Data.Sequence (Seq)

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    frameState      ::   FrameState IDEM         -- ^ state of the windows framework
,   recentPanes     ::   [PaneName]              -- ^ a list of panes which were selected last
,   specialKeys     ::   SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::   SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy           ::   CandyTable              -- ^ table for source candy
,   prefs           ::   Prefs                   -- ^ configuration preferences
,   workspace       ::   Maybe Workspace         -- ^ may be a workspace (set of packages)
,   activePack      ::   Maybe IDEPackage
,   activeExe       ::   Maybe Text
,   bufferProjCache ::   Map FilePath [IDEPackage] -- ^ cache the associated packages for a file
,   allLogRefs      ::   Seq LogRef
,   currentEBC      ::   (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   currentHist     ::   Int
,   systemInfo      ::   Maybe GenScope              -- ^ the system scope
,   packageInfo     ::   Maybe (GenScope, GenScope) -- ^ the second are the imports
,   workspaceInfo   ::   Maybe (GenScope, GenScope) -- ^ the second are the imports
,   workspInfoCache ::   PackageDescrCache
,   handlers        ::   Map Text [(Unique, IDEEvent -> IDEM IDEEvent)] -- ^ event handling table
,   currentState    ::   IDEState
,   guiHistory      ::   (Bool,[GUIHistory],Int)
,   findbar         ::   (Bool,Maybe (Toolbar,ListStore Text))
,   toolbar         ::   (Bool,Maybe Toolbar)
,   recentFiles     ::   [FilePath]
,   recentWorkspaces ::  [FilePath]
,   runningTool     ::   Maybe ProcessHandle
,   packageToGhci   ::   Map IDEPackage ToolState
-- ,   debugState      ::   Maybe (IDEPackage, ToolState)
,   completion      ::   ((Int, Int), Maybe CompletionWindow)
,   yiControl       ::   Yi.Control
,   serverQueue     ::   Maybe (MVar (ServerCommand, ServerAnswer -> IDEM ()))
,   server          ::   Maybe Handle
,   hlintQueue      ::   Maybe (TVar [Either FilePath FilePath])
,   vcsData         ::   (Map FilePath MenuItem, Maybe (Maybe Text)) -- menus for packages, password
,   logLaunches     ::   Map.Map Text LogLaunchData
,   autoCommand     ::   IDEAction -- ^ Action to be run when ghci reloads
,   autoURI         ::   Maybe Text
} --deriving Show

--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

--
-- | The IDE Monad
--
type IDEM = ReaderT IDERef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()


data IDEState =
        -- | Leksah is in startup mode
        IsStartingUp
        -- | Leksah is about to go down
    |   IsShuttingDown
        -- | Leksah is running
    |   IsRunning
        -- | The flipper is used to switch between sources
    |   IsFlipping TreeView
        -- | The completion feature is used
    |   IsCompleting Connections


class (Functor m, Monad m, MonadIO m) => MonadIDE m where
    liftIDE :: IDEM a -> m a

instance MonadIDE IDEM where
    liftIDE = id

instance MonadIDE WorkspaceM where
    liftIDE = lift

(?>>=) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
a ?>>= b = do
    mA <- a
    case mA of
        Just v -> b v
        Nothing -> return ()

-- ---------------------------------------------------------------------
-- Monad for Gtk events (use onIDE instead of on)
--
type IDEEventM t = ReaderT IDERef (ReaderT (Ptr t) IO)

instance MonadIDE (IDEEventM t) where
    liftIDE f = do
        ideR <- ask
        liftIO $ runReaderT f ideR

-- ---------------------------------------------------------------------
-- Monad for functions that need an open workspace
--
type WorkspaceM = ReaderT Workspace IDEM
type WorkspaceAction = WorkspaceM ()

runWorkspace :: WorkspaceM a -> Workspace -> IDEM a
runWorkspace = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need an active package
--
type PackageM = ReaderT IDEPackage WorkspaceM
type PackageAction = PackageM ()

instance MonadIDE PackageM where
    liftIDE = lift . lift

runPackageM :: PackageM a -> IDEPackage -> WorkspaceM a
runPackageM = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need to use the GHCi debugger
--
type DebugM = ReaderT (IDEPackage, ToolState) IDEM
type DebugAction = DebugM ()

runDebugM :: DebugM a -> (IDEPackage, ToolState) -> IDEM a
runDebugM = runReaderT

-- ---------------------------------------------------------------------
-- Events which can be signalled and handled
--

data IDEEvent  =
        InfoChanged Bool-- is it the initial = True else False
    |   UpdateWorkspaceInfo
    |   SelectInfo Text Bool -- navigate to source (== True)
    |   SelectIdent Descr
    |   LogMessage Text LogTag
    |   RecordHistory GUIHistory
    |   Sensitivity [(SensitivityMask,Bool)]
    |   SearchMeta Text
    |   StartFindInitial
    |   GotoDefinition Descr
    |   LoadSession FilePath
    |   SaveSession FilePath
    |   UpdateRecent
    |   VariablesChanged
    |   ErrorChanged Bool
    |   ErrorAdded Bool Int LogRef
    |   CurrentErrorChanged (Maybe LogRef)
    |   BreakpointChanged
    |   CurrentBreakChanged (Maybe LogRef)
    |   TraceChanged
    |   GetTextPopup (Maybe (IDERef -> Menu -> IO ()))
    |   StatusbarChanged [StatusbarCompartment]
    |   WorkspaceChanged Bool Bool -- ^ showPane updateFileCache
    |   SelectSrcSpan (Maybe SrcSpan)
    |   SavedFile FilePath

instance Event IDEEvent Text where
    getSelector (InfoChanged _)         =   "InfoChanged"
    getSelector UpdateWorkspaceInfo     =   "UpdateWorkspaceInfo"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _ _)        =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
    getSelector (RecordHistory _)       =   "RecordHistory"
    getSelector (Sensitivity _)         =   "Sensitivity"
    getSelector (SearchMeta _)          =   "SearchMeta"
    getSelector (StartFindInitial)      =   "StartFindInitial"
    getSelector (GotoDefinition _)      =   "GotoDefinition"
    getSelector (LoadSession _)         =   "LoadSession"
    getSelector (SaveSession _)         =   "SaveSession"
    getSelector UpdateRecent            =   "UpdateRecent"
    getSelector VariablesChanged        =   "VariablesChanged"
    getSelector (ErrorChanged _)        =   "ErrorChanged"
    getSelector (ErrorAdded _ _ _)      =   "ErrorAdded"
    getSelector (CurrentErrorChanged _) =   "CurrentErrorChanged"
    getSelector BreakpointChanged       =   "BreakpointChanged"
    getSelector (CurrentBreakChanged _) =   "CurrentBreakChanged"
    getSelector TraceChanged            =   "TraceChanged"
    getSelector (GetTextPopup _)        =   "GetTextPopup"
    getSelector (StatusbarChanged _)    =   "StatusbarChanged"
    getSelector (WorkspaceChanged _ _)  =   "WorkspaceChanged"
    getSelector (SelectSrcSpan _)       =   "SelectSrcSpan"
    getSelector (SavedFile _)           =   "SavedFile"

instance EventSource IDERef IDEEvent IDEM Text where
    canTriggerEvent _ "InfoChanged"         = True
    canTriggerEvent _ "UpdateWorkspaceInfo" = True
    canTriggerEvent _ "LogMessage"          = True
    canTriggerEvent _ "SelectInfo"          = True
    canTriggerEvent _ "SelectIdent"         = True
    canTriggerEvent _ "RecordHistory"       = True
    canTriggerEvent _ "Sensitivity"         = True
    canTriggerEvent _ "DescrChoice"         = True
    canTriggerEvent _ "SearchMeta"          = True
    canTriggerEvent _ "StartFindInitial"    = True
    canTriggerEvent _ "SearchSymbolDialog"  = True
    canTriggerEvent _ "GotoDefinition"      = True
    canTriggerEvent _ "LoadSession"         = True
    canTriggerEvent _ "SaveSession"         = True
    canTriggerEvent _ "UpdateRecent"        = True
    canTriggerEvent _ "VariablesChanged"    = True
    canTriggerEvent _ "ErrorChanged"        = True
    canTriggerEvent _ "ErrorAdded"          = True
    canTriggerEvent _ "CurrentErrorChanged" = True
    canTriggerEvent _ "BreakpointChanged"   = True
    canTriggerEvent _ "CurrentBreakChanged" = True
    canTriggerEvent _ "TraceChanged"        = True
    canTriggerEvent _ "GetTextPopup"        = True
    canTriggerEvent _ "StatusbarChanged"    = True
    canTriggerEvent _ "WorkspaceChanged"    = True
    canTriggerEvent _ "SelectSrcSpan"       = True
    canTriggerEvent _ "SavedFile"           = True
    canTriggerEvent _ _                   = False
    getHandlers ideRef = do
        ide <- liftIO $ readIORef ideRef
        return (handlers ide)
    setHandlers ideRef nh = do
        ide <- liftIO $ readIORef ideRef
        liftIO $ writeIORef ideRef (ide {handlers= nh})
    myUnique _ =
        liftIO newUnique

instance EventSelector Text

-- ---------------------------------------------------------------------
-- IDEPackages
--
data IDEPackage     =   IDEPackage {
    ipdPackageId       ::   PackageIdentifier
,   ipdCabalFile       ::   FilePath
,   ipdDepends         ::   [Dependency]
,   ipdModules         ::   Map ModuleName BuildInfo
,   ipdHasLibs         ::   Bool
,   ipdExes            ::   [Text]
,   ipdTests           ::   [Text]
,   ipdBenchmarks      ::   [Text]
,   ipdMain            ::   [(FilePath, BuildInfo, Bool)]
,   ipdExtraSrcs       ::   Set FilePath
,   ipdSrcDirs         ::   [FilePath]
,   ipdExtensions      ::   [Extension]
,   ipdConfigFlags     ::   [Text]
,   ipdBuildFlags      ::   [Text]
,   ipdTestFlags       ::   [Text]
,   ipdHaddockFlags    ::   [Text]
,   ipdExeFlags        ::   [Text]
,   ipdInstallFlags    ::   [Text]
,   ipdRegisterFlags   ::   [Text]
,   ipdUnregisterFlags ::   [Text]
,   ipdSdistFlags      ::   [Text]
,   ipdSandboxSources  ::   [IDEPackage]
} deriving Eq

instance Show IDEPackage where
    show p = show "IDEPackage for " ++ (render . disp) (ipdPackageId p)

instance Ord IDEPackage where
    compare x y     =   compare (ipdPackageId x) (ipdPackageId y)

ipdBuildDir :: IDEPackage -> FilePath
ipdBuildDir = dropFileName . ipdCabalFile

ipdAllDirs :: IDEPackage -> [FilePath]
ipdAllDirs p = ipdBuildDir p : (ipdSandboxSources p >>= ipdAllDirs)

-- ---------------------------------------------------------------------
-- Workspace
--
data Workspace = Workspace {
    wsVersion       ::   Int
,   wsSaveTime      ::   Text
,   wsName          ::   Text
,   wsFile          ::   FilePath
,   wsPackages      ::   [IDEPackage]
,   wsPackagesFiles ::   [FilePath]
,   wsActivePackFile::   Maybe FilePath
,   wsActiveExe     ::   Maybe Text
,   wsNobuildPack   ::   [IDEPackage]
,   packageVcsConf  ::   Map FilePath VCSConf -- ^ (FilePath to package, Version-Control-System Configuration)
} deriving Show

-- | Includes sandbox sources
wsAllPackages :: Workspace -> [IDEPackage]
wsAllPackages w = nubBy ((==) `on` ipdCabalFile) $ wsPackages w ++ (wsPackages w >>= ipdSandboxSources)

-- ---------------------------------------------------------------------
-- Other data structures which are used in the state
--

--
-- | ActionDescr is a data structure from which GtkActions are build, which are used for
--   menus, toolbars, and accelerator keystrokes
--
data ActionDescr alpha = AD {
    name        ::   ActionString
,   label       ::   Text
,   tooltip     ::   Maybe Text
,   stockID     ::   Maybe Text
,   action      ::   ReaderT alpha IO ()
,   accelerator ::   [KeyString]
,   isToggle    ::   Bool
}

type ActionString = Text
type KeyString = Text

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
        prefsFormat         ::   Int
    ,   prefsSaveTime       ::   Text
    ,   showLineNumbers     ::   Bool
    ,   rightMargin         ::   (Bool, Int)
    ,   tabWidth            ::   Int
    ,   wrapLines           ::   Bool
    ,   sourceCandy         ::   (Bool,Text)
    ,   keymapName          ::   Text
    ,   forceLineEnds       ::   Bool
    ,   removeTBlanks       ::   Bool
    ,   textviewFont        ::   Maybe Text
    ,   sourceStyle         ::   (Bool, Text)
    ,   foundBackgroundLight      ::   Color
    ,   matchBackgroundLight      ::   Color
    ,   contextBackgroundLight    ::   Color
    ,   breakpointBackgroundLight ::   Color
    ,   lintBackgroundLight       ::   Color
    ,   foundBackgroundDark       ::   Color
    ,   matchBackgroundDark       ::   Color
    ,   contextBackgroundDark     ::   Color
    ,   breakpointBackgroundDark  ::   Color
    ,   lintBackgroundDark        ::   Color
    ,   autoLoad            ::   Bool
    ,   textEditorType      ::   Text
    ,   logviewFont         ::   Maybe Text
    ,   defaultSize         ::   (Int,Int)
    ,   browser             ::   Text
    ,   pathForCategory     ::   [(Text, PanePath)]
    ,   defaultPath         ::   PanePath
    ,   categoryForPane     ::   [(Text, Text)]
    ,   packageBlacklist    ::   [Dependency]
    ,   collectAtStart      ::   Bool
    ,   useCtrlTabFlipping  ::   Bool
    ,   docuSearchURL       ::   Text
    ,   completeRestricted  ::   Bool
    ,   saveAllBeforeBuild  ::   Bool
    ,   jumpToWarnings      ::   Bool
    ,   useVado             ::   Bool
    ,   useCabalDev         ::   Bool
    ,   backgroundBuild     ::   Bool
    ,   runUnitTests        ::   Bool
    ,   makeMode            ::   Bool
    ,   singleBuildWithoutLinking :: Bool
    ,   dontInstallLast     ::   Bool
    ,   printEvldWithShow   ::   Bool
    ,   breakOnException    ::   Bool
    ,   breakOnError        ::   Bool
    ,   printBindResult     ::   Bool
    ,   serverIP            ::   Text
    ,   showHiddenFiles     ::   Bool
            -- As well used by server
    ,   serverPort          ::   Int
    ,   sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retrieveURL         ::   Text
    ,   retrieveStrategy    ::   RetrieveStrategy
    ,   endWithLastConn     ::   Bool
} deriving(Eq,Show)

cabalCommand :: Prefs -> FilePath
cabalCommand p = if useCabalDev p then "cabal-dev" else "cabal"

data EditorStyle = EditorStyle { styleName    :: Maybe Text
                               , preferDark   :: Bool
                               , foundBG      :: (Color, Color)
                               , matchBG      :: (Color, Color)
                               , contextBG    :: (Color, Color)
                               , breakpointBG :: (Color, Color)
                               , lintBG       :: (Color, Color)
                               }

editorStyle :: Bool -> Prefs -> EditorStyle
editorStyle preferDark prefs = EditorStyle { styleName = case sourceStyle prefs of
                                                        (False,_) -> Nothing
                                                        (True,v)  -> Just v
                                           , preferDark = preferDark
                                           , foundBG      = (foundBackgroundDark      prefs, foundBackgroundLight      prefs)
                                           , matchBG      = (matchBackgroundDark      prefs, matchBackgroundLight      prefs)
                                           , contextBG    = (contextBackgroundDark    prefs, contextBackgroundLight    prefs)
                                           , breakpointBG = (breakpointBackgroundDark prefs, breakpointBackgroundLight prefs)
                                           , lintBG       = (lintBackgroundDark       prefs, lintBackgroundLight       prefs)
                                           }

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

#ifndef LEKSAH_WITH_YI
instance Ord Modifier
    where compare a b = compare (fromEnum a) (fromEnum b)
#endif

-- Version-Control-System Configuration
type VCSConf = (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool)

--
-- | Other types
--

data LogLaunchData = LogLaunchData {
    logLaunch :: LogLaunch
,   mbPid :: Maybe ProcessHandle
}

data LogLaunch = LogLaunch {
    logBuffer   :: TextBuffer
} deriving Typeable


-- Order determines priority of the icons in the gutter
data LogRefType = ContextRef | BreakpointRef | ErrorRef | TestFailureRef | WarningRef | LintRef
    deriving (Eq, Ord, Show, Enum, Bounded)


-- | Represents a message about a part of the source code
data LogRef = LogRef {
    logRefSrcSpan       ::   SrcSpan
,   logRefPackage       ::   IDEPackage
,   refDescription      ::   Text
,   logRefIdea          ::   Maybe (Text, Idea)
,   logLines            ::   Maybe (Int, Int)
,   logRefType          ::   LogRefType
} deriving (Eq)

instance Show LogRef where
    show lr = T.unpack (refDescription lr) ++ displaySrcSpan (logRefSrcSpan lr)

displaySrcSpan s = srcSpanFilename s ++ ":" ++
    if srcSpanStartLine s == srcSpanEndLine s
        then show (srcSpanStartLine s) ++ ":" ++
            if srcSpanStartColumn s == srcSpanEndColumn s
                then show (srcSpanStartColumn s)
                else show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)
        else show (srcSpanStartLine s) ++ ":" ++
            show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)

logRefRootPath :: LogRef -> FilePath
logRefRootPath = ipdBuildDir . logRefPackage

logRefFilePath :: LogRef -> FilePath
logRefFilePath = srcSpanFilename . logRefSrcSpan

logRefFullFilePath :: LogRef -- ^ The log ref
    -> FilePath -- ^ the result
logRefFullFilePath lr = logRefRootPath lr </> logRefFilePath lr

isError :: LogRef -> Bool
isError = (== ErrorRef) . logRefType

isBreakpoint :: LogRef -> Bool
isBreakpoint = (== BreakpointRef) . logRefType

isContext :: LogRef -> Bool
isContext = (== ContextRef) . logRefType

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString (Color r g b) = '#' : pad (showHex r "")
                                  ++ pad (showHex g "")
                                  ++ pad (showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s


newtype CandyTable      =   CT (CandyTableForth,CandyTableBack)

type CandyTableForth    =   [(Bool,Text,Text)]

type CandyTableBack     =   [(Text,Text,Int)]

newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe Text)])

type SpecialKeyTable alpha  =   Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) (ActionDescr alpha))

type SpecialKeyCons  alpha  =   Maybe (Map (KeyVal, [Modifier]) (ActionDescr alpha), Text)

data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag

-- | the first one is the new and the second the old state
type GUIHistory = (GUIHistory', GUIHistory')

data GUIHistory' =
        ModuleSelected  {
            moduleS :: Maybe ModuleName
        ,   facetS  :: Maybe Text}
    |   ScopeSelected {
            scope   :: Scope
        ,   blacklist :: Bool}
    |   InfoElementSelected {
            mbInfo  :: Maybe Descr}
    |   PaneSelected {
            paneN   :: Maybe Text}
   deriving (Eq, Ord, Show)

data SensitivityMask =
        SensitivityForwardHist
    |   SensitivityBackwardHist
    |   SensitivityProjectActive
    |   SensitivityWorkspaceOpen
    |   SensitivityError
    |   SensitivityEditor
    |   SensitivityInterpreting

   deriving (Eq, Ord, Show)

data SearchMode = Exact {caseSense :: Bool} | Prefix {caseSense :: Bool}
                | Regex {caseSense :: Bool}
    deriving (Eq,Ord,Read,Show)

data CompletionWindow = CompletionWindow {
    cwWindow :: Window,
    cwTreeView :: TreeView,
    cwListStore :: ListStore Text}

data StatusbarCompartment =
        CompartmentCommand Text
    |   CompartmentPane (Maybe (IDEPane IDEM))
    |   CompartmentPackage Text
    |   CompartmentState Text
    |   CompartmentOverlay Bool
    |   CompartmentBufferPos (Int,Int)
    |   CompartmentBuild Bool
    |   CompartmentCollect Bool

type PackageDescrCache = Map PackageIdentifier ModuleDescrCache
type ModuleDescrCache = Map ModuleName (UTCTime, Maybe FilePath, ModuleDescr)
