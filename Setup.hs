import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.System (OS(..), buildOS)
import System.IO (IOMode(..), hPutStrLn, withFile)

main = defaultMainWithHooks $ simpleUserHooks {
         postConf = configure
       }

configure :: Args
          -> ConfigFlags
          -> PackageDescription
          -> LocalBuildInfo
          -> IO ()
configure _ _ _ _ =
  withFile "config.h" WriteMode (go buildOS)
    where
      go os handle =
        let str =
              case os of
                Linux   -> "__LINUX__"
                OSX     -> "__MACOSX__"
                Windows -> "__WINDOWS__" in
        hPutStrLn handle ("#define " ++ str)
