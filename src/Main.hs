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
import           Data.Monoid
import           Data.Monoid.Unicode
import           Data.Text.Lazy              as T hiding (unpack)
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


commands ∷ [(ByteString, AppSettings → HookData → IO Text)]
commands =
  [ ("bashthis:", addNew) ]


data HookData = HookData { hookDataToken ∷ ByteString
                         , command       ∷ ByteString
                         , text          ∷ ByteString
                         } deriving (Show)


data AppSettings = AppSettings { port             ∷ Int
                               , appSettingsToken ∷ Maybe ByteString
                               , password         ∷ String
                               , username         ∷ String
                               , uri              ∷ URI
                               , minQuoteLength   ∷ Maybe Int
                               , logfile          ∷ Maybe FilePath
                               } deriving (Show)


writeLog ∷ AppSettings → String → IO ()
writeLog (AppSettings { logfile = (Just f) }) =
  Prelude.writeFile f
writeLog _ = const $ return ()


showLog ∷ Show s ⇒ AppSettings → s → IO ()
showLog set = writeLog set ∘ show


addNew ∷ AppSettings → HookData → IO Text
addNew
  settings@(AppSettings { username = user, password = passwd, uri = uri, minQuoteLength = mql })
  (HookData { command = cmd, text = text })
  = if lengthVerifier quote
    then browse $ do
      setAuthorityGen (\_ _ → return $ return (user, passwd))
      setAllowBasicAuth True
      request req
      liftIO $ writeLog settings "success"
      return "Yeey, new quotes!!! Thank you 😃"
    else
      return $ "Your quote is too short, the bash will reject it 😐. "
        ⊕ maybe "" (\req -> "Just make it like at least " ⊕ T.pack (show (req - C.length quote)) ⊕ " characters longer.") mql
    where
      quote = C.dropWhile isSpace $ C.drop (C.length cmd) text
      lengthVerifier = maybe (const True) (\a b -> a ≤ C.length b) mql
      req = formToRequest body
      -- authority = AuthBasic { auRealm = realm, auUsername = user, auPassword = passwd, auSite = uri }
      body =
        Form
          POST
          uri
          [ ("rash_quote", unpack quote)
          , ("submit", "Add Quote")
          ]


instance FromJSON AppSettings where
  parseJSON (Object o) = AppSettings
    <$> o .: "port"
    ⊛ (fmap $ fmap C.pack) (o .:? "token")
    ⊛ o .: "password"
    ⊛ o .: "username"
    ⊛ fmap (fromJust ∘ parseURI) (o .: "target")
    ⊛ o .:? "min_quote_length"
    ⊛ o .:? "logfile"


parseData ∷ [(ByteString, ByteString)] → Maybe HookData
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
      if verifier token
        then
          case lookup command commands of
            Nothing → respondFail
            Just action → do
              showLog settings postData
              action settings postData ≫= respondSuccess
        else respondFail

  where
    verifier = maybe (const True) (≡) expectedToken
    respondSuccess message =
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ encodeUtf8 $ "{\"text\":\"" ⊕ message ⊕ "\"}"
    respondFail =
      respond $ responseLBS badRequest400 [] $ encodeUtf8 "{\"text\": \"Sorry, something went wrong 😐\"}"


main ∷ IO ()
main = do
  [settingsFile] ← getArgs
  sf ← decodeFile settingsFile
  case sf of
    Nothing → putStrLn "could not read settings"
    Just conf → do
      writeLog conf "Starting server with config:"
      showLog conf conf
      run (port conf) (app conf)
