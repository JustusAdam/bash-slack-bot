{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Unicode
import           Data.ByteString.Char8       as C hiding (putStrLn, unlines)
import           Data.Char
import           Data.Maybe
import           Data.Text.Lazy.Encoding
import           Data.Yaml
import           Network.Browser
import           Network.HTTP                hiding (password, port)
import           Network.HTTP.Auth
import           Network.HTTP.Types          hiding (POST)
import           Network.URI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           Prelude.Unicode
import           System.Environment
import           Text.Printf


tryCommit = True


commands ∷ [(ByteString, AppSettings → HookData → IO ())]
commands =
  [ ("bashthis:", addNew) ]


data HookData = HookData { hookDataToken ∷ ByteString
                         , command       ∷ ByteString
                         , text          ∷ ByteString
                         } deriving (Show)


data AppSettings = AppSettings { port             ∷ Int
                               , appSettingsToken ∷ ByteString
                               , password         ∷ String
                               , username         ∷ String
                               , uri              ∷ URI
                               } deriving (Show)


addNew
  (AppSettings { username = user, password = passwd, uri = uri })
  (HookData { command = cmd, text = text })
  = browse $ do
      setAuthorityGen (\_ _ → return $ return (user, passwd))
      setAllowBasicAuth True
      request req
      liftIO $ putStrLn "success"
    where
      req = formToRequest body
      -- authority = AuthBasic { auRealm = realm, auUsername = user, auPassword = passwd, auSite = uri }
      body =
        Form
          POST
          uri
          [ ("rash_quote", unpack $ C.dropWhile isSpace $ C.drop (C.length cmd) text)
          , ("submit", "Add Quote")
          ]


instance FromJSON AppSettings where
  parseJSON (Object o) = AppSettings
    <$> o .: "port"
    ⊛ fmap pack (o .: "token")
    ⊛ o .: "password"
    ⊛ o .: "username"
    ⊛ fmap (fromJust ∘ parseURI) (o .: "target")


parseData params = HookData
  <$> lookup "token" params
  ⊛ lookup "trigger_word" params
  ⊛ lookup "text" params


app ∷ AppSettings → Application
app settings@(AppSettings { appSettingsToken = expectedToken }) req respond = do
  mpostData ← parseData ∘ fst <$> parseRequestBody lbsBackEnd req

  case mpostData of
    Nothing → respondFail
    Just postData@(HookData { hookDataToken = token, command = command, text = text }) →
      if expectedToken ≢ token
        then respondFail
        else
          case lookup command commands of
            Nothing → respondFail
            Just action → do
              print postData
              action settings postData
              respondSuccess

  where
    respondSuccess =
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ encodeUtf8 "{\"text\":\"Yeey, new quotes!!! Thank you 😃\"}"
    respondFail =
      respond $ responseLBS badRequest400 [] $ encodeUtf8 "{\"text\": \"Sorry, something went wrong 😐\"}"


main = do
  [settingsFile] ← getArgs
  sf ← decodeFile settingsFile
  case sf of
    Nothing → putStrLn "could not read settings"
    Just conf → do
      putStrLn "Starting server with config:"
      print conf
      run (port conf) (app conf)
