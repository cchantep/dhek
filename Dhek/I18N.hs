{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dhek.I18N where

import Data.List (dropWhileEnd)
import System.Environment
import System.Process

import Data.Text (Text, unpack, pack)
import Distribution.System (OS(..), buildOS)
import System.FilePath
import Text.Shakespeare.I18N

data Dhek = Dhek

mkMessage "Dhek"
          (joinPath ["messages", "main"])
          "en"

mkI18N :: IO (DhekMessage -> String)
mkI18N = fmap (\l -> unpack . renderMessage Dhek [l]) determineLang

determineLang :: IO Text
determineLang =
    case buildOS of
        Linux   -> determineLinux
        OSX     -> determineOSX
        Windows -> determineWindows
        _       -> return "en"
  where
    determineLinux = fmap (maybe "en" (pack . take 2)) (lookupEnv "LANG")

    determineOSX = fmap (pack . take 2)
                   (readProcess "defaults" ["read", "-g", "AppleLocale"] [])

    determineWindows =
        let action =
                readProcess
                "reg"
                [ "query"
                , "hklm\\system\\controlset001\\control\\nls\\language"
                , "/v"
                , "Installlanguage"
                ]
                [] in
        fmap fromLangCode action

takeEnd :: Int -> [a] -> [a]
takeEnd n = snd . foldr go (0,[]) where
  go a (c,as)
      | c == n    = (c, as)
      | otherwise = (c+1, a:as)

fromLangCode :: String -> Text
fromLangCode xs
    | dropWhileEnd ('\n' ==) (takeEnd 6 xs) == "040C" = "fr"
    | otherwise                                       = "en"
