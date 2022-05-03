{-# LANGUAGE ForeignFunctionInterface #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Trustworthy #-}
#endif

module Trace.Hpc.Reflect
  ( clearTix
  , examineTix
  , updateTix
  ) where

import Trace.Hpc.Tix

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable ( Storable(..) )
import Data.Word
import Trace.Hpc.Util
import System.IO.Unsafe

#include "Rts.h"

foreign import ccall unsafe hs_hpc_rootModule :: IO (Ptr ())

modInfo :: [ModuleInfo]
modInfo = unsafePerformIO $ do
      ptr <- hs_hpc_rootModule
      moduleInfoList ptr

data ModuleInfo = ModuleInfo String Word32 Hash (Ptr Word64) (Ptr Word64) (Ptr Word64)

moduleInfoList :: Ptr () -> IO [ModuleInfo]
moduleInfoList ptr
  | ptr == nullPtr = return []
  | otherwise = do
        cModName  <- (#peek HpcModuleInfo, modName) ptr
        modName   <- peekCString cModName
        tickCount <- (#peek HpcModuleInfo, tickCount) ptr
        hashNo    <- (#peek HpcModuleInfo, hashNo) ptr
        tixArr    <- (#peek HpcModuleInfo, tixArr) ptr
        trxInfo   <- (#peek HpcModuleInfo, trxInfo) ptr
        trxArr    <- (#peek HpcModuleInfo, trxArr) ptr
        next      <- (#peek HpcModuleInfo, next) ptr
        rest      <- moduleInfoList next
        return $ ModuleInfo modName tickCount (toHash (hashNo :: Int)) tixArr trxInfo trxArr : rest

clearTix :: IO ()
clearTix = do
      sequence_ [ pokeArray ptr $ take (fromIntegral count) $ repeat 0
                | ModuleInfo _mod count _hash ptr _info _trx <- modInfo
                ]
      return ()


examineTix :: IO Tix
examineTix = do
      mods <- sequence [ do tixs <- peekArray (fromIntegral count) ptr
                            info <- peekArray (fromIntegral 2) info
                            trx <- peekArray (fromIntegral (info !! 1)) trx
                            return $ TixModule mod' hash (fromIntegral count)
                                   (map fromIntegral tixs)
                                   (map fromIntegral info)
                                   (map fromIntegral trx)
                       | (ModuleInfo mod' count hash ptr info trx) <- modInfo
                       ]
      return $ Tix mods

-- requirement that the tix be of the same shape as the
-- internal tix.
updateTix :: Tix -> IO ()
updateTix (Tix modTixes)
  | length modTixes /= length modInfo = error "updateTix failed"
  | otherwise = do
      sequence_ [ pokeArray ptr $ map fromIntegral tixs
                | (ModuleInfo mod1 count1 hash1 ptr info1 trx1,
                   TixModule mod2 hash2 count2 tixs info2 trx2) <- zip modInfo modTixes
                , if mod1 /= mod2
                || (fromIntegral count1) /= count2
                || hash1 /= hash2
                || length tixs /= count2
                  then error "updateTix failed"
                  else True
                ]
      return ()
