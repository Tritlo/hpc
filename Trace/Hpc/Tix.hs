{-# LANGUAGE Safe, DeriveGeneric, StandaloneDeriving, CPP #-}
------------------------------------------------------------
-- Andy Gill and Colin Runciman, June 2006
------------------------------------------------------------

-- | Datatypes and file-access routines for the tick data file
-- (@.tix@) used by Hpc.
module Trace.Hpc.Tix(Tix(..), TixModule(..),
                     tixModuleName, tixModuleHash, tixModuleTixs,
#if __GLASGOW_HASKELL__ >= 963
                     tixModuleTraceInfo, tixModuleTrace,
#endif
                     readTix, writeTix, getTixFileName) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.FilePath (replaceExtension)

import Trace.Hpc.Util (Hash, catchIO, readFileUtf8, writeFileUtf8)

-- | 'Tix' is the storage format for our dynamic information about
-- what boxes are ticked.
data Tix = Tix [TixModule]
        deriving (Read, Show, Eq)

-- | @since 0.6.2.0
deriving instance (Generic Tix)
-- | @since 0.6.2.0
instance NFData Tix

data TixModule = TixModule
                 String    --  module name
                 Hash      --  hash number
                 Int       --  length of Tix list (allows pre-allocation at parse time).
                 [Integer] --  actual ticks
#if __GLASGOW_HASKELL__ >= 963
                 [Integer] --  current trace posistion
                 [Integer] -- traces
#endif
        deriving (Read, Show, Eq)

-- | @since 0.6.2.0
deriving instance (Generic TixModule)
-- | @since 0.6.2.0
instance NFData TixModule

-- TODO: Turn extractors below into proper 'TixModule' field-labels
tixModuleName :: TixModule -> String
tixModuleHash :: TixModule -> Hash
tixModuleTixs :: TixModule -> [Integer]
#if __GLASGOW_HASKELL__ >= 963
tixModuleName (TixModule nm _ _ _ _ _) = nm
tixModuleHash (TixModule _ h  _ _ _ _) = h
tixModuleTixs (TixModule  _ _ _ tixs _ _) = tixs
tixModuleTraceInfo :: TixModule -> [Integer]
tixModuleTrace :: TixModule -> [Integer]
tixModuleTraceInfo (TixModule  _ _ _ _ info _) = info
tixModuleTrace (TixModule  _ _ _ _ _ trace) = trace
#else
tixModuleName (TixModule nm _ _ _ ) = nm
tixModuleHash (TixModule _ h  _ _ ) = h
tixModuleTixs (TixModule  _ _ _ tixs) = tixs
#endif

-- We /always/ read and write Tix from the current working directory.

-- | Read a @.tix@ File.
readTix :: FilePath
        -> IO (Maybe Tix)
readTix tixFilename =
  catchIO (fmap (Just . read) $ readFileUtf8 tixFilename)
          (const $ return Nothing)

-- | Write a @.tix@ File.
writeTix :: FilePath
         -> Tix
         -> IO ()
writeTix name tix = writeFileUtf8 name (show tix)

-- | 'getTixFullName' takes a binary or @.tix@-file name,
-- and normalizes it into a @.tix@-file name.
--
-- > getTixFileName "example.hs" == "example.tix"
-- > getTixFileName "example.tar.gz" == "example.tar.tix"
-- > getTixFileName "example.tix" == "example.tix"
getTixFileName :: FilePath -> FilePath
getTixFileName str = replaceExtension str "tix"
