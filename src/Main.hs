{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}


import Network.Wai
import Network.HTTP.Types
import Network.HTTP hiding (password, port)
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Data.ByteString.Char8 hiding (putStrLn)
import Prelude.Unicode
import Data.Yaml
import System.Environment
import Control.Monad
import Control.Applicative
import Control.Applicative.Unicode


commands :: [(ByteString, AppSettings → HookData → IO ())]
commands =
  [ ("bashthis:", addNew) ]

--
-- rash_quote = text
-- "Add Quote"
--

data HookData = HookData { hookDataToken :: ByteString
                         , command :: ByteString
                         , text :: ByteString
                         } deriving (Show)


data AppSettings = AppSettings { port :: Int
                               , appSettingsToken :: ByteString
                               , password :: String
                               , username :: String
                               } deriving (Show)



addNew (AppSettings { username = user, password = passwd }) (HookData { text = text }) = do
  let url = "http://" ⧺ user ⧺ ":" ⧺ passwd ⧺ "@bash.fsrleaks.de/?add"
  let contentType = "application/x-www-form-urlencoded"
  let body = "rash_quote=" ++ unpack text ++ "\nsubmit=Add Quote\n"
  print =<< simpleHTTP (
    postRequestWithBody
      url
      contentType
      body
    )
  return ()




instance FromJSON AppSettings where
  parseJSON (Object o) = AppSettings
    <$> o .: "port"
    <*> fmap pack (o .: "token")
    <*> o .: "password"
    <*> o .: "username"


parseData params = HookData
  <$> lookup "token" params
  <*> lookup "trigger_word" params
  <*> lookup "text" params


app :: AppSettings -> Application
app settings@(AppSettings { appSettingsToken = expectedToken }) req respond = do
  mpostData ← parseData ∘ fst <$> parseRequestBody lbsBackEnd req

  case mpostData of
    Nothing -> respondFail
    Just postData@(HookData { hookDataToken = token, command = command, text = text }) ->
      if expectedToken ≢ token
        then respondFail
        else
          case lookup command commands of
            Nothing -> respondFail
            Just action -> do
              print postData
              action settings postData
              respond $ responseLBS status200 [("Content-Type", "application/json")] "Yeey, new quotes!!!"
  where
    respondFail =
      respond $ responseLBS badRequest400 [] ""


main = do
  [settingsFile] <- getArgs
  sf <- decodeFile settingsFile
  case sf of
    Nothing -> putStrLn "could not read settings"
    Just conf -> do
      putStrLn "Starting server with config:"
      print conf
      run (port conf) (app conf)
