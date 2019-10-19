module Main (main) where

import Distribution.Simple
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import Hpack

main :: IO ()
main = do
  let myHook = simpleUserHooks {
    preBuild = pbHpack,
    preTest = pbHpack
  }
  defaultMainWithHooks myHook

pbHpack :: Args -> a -> IO HookedBuildInfo
pbHpack _ _ = hpack Verbose defaultOptions >> pure emptyHookedBuildInfo
