{-# LANGUAGE DeriveDataTypeable #-}

module Hydra.Architecture.Plugin (
  plugin,
  Architecture (..),
) where

import Control.Monad (unless)
import Data.Data hiding (TyCon)
import GHC.IO.IOMode (IOMode (AppendMode))
import GhcPlugins hiding ((<>))
import Hydra.Architecture.Annotations
import System.IO (hPutStrLn, withFile)
import Prelude

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Architecture Annotations" pass : todo)

data Annotable = TypeCtor TyCon | Binder CoreBind

pass :: ModGuts -> CoreM ModGuts
pass g = do
  dflags <- getDynFlags
  mapM_ (printAnn dflags g) ((Binder <$> mg_binds g) <> (TypeCtor <$> mg_tcs g))
  return g
 where
  printAnn :: DynFlags -> ModGuts -> Annotable -> CoreM ()
  printAnn dflags guts (Binder (NonRec b _)) = do
    anns <- annotationsOn guts (varUnique b) :: CoreM [Architecture]
    unless (null anns) $
      liftIO $ do
        withFile "annotations" AppendMode $ \hdl -> do
          hPutStrLn hdl $ "Annotated binding found: " ++ showSDoc dflags (ppr b)
  printAnn dflags guts (TypeCtor tycon) = do
    anns <- annotationsOn guts (getUnique tycon) :: CoreM [Architecture]
    unless (null anns) $
      liftIO $ do
        withFile "annotations" AppendMode $ \hdl -> do
          hPutStrLn hdl $ "Annotated binding found: " ++ showSDoc dflags (ppr tycon)
  printAnn _ _ _ = pure ()

annotationsOn :: Data a => ModGuts -> Unique -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] bndr
