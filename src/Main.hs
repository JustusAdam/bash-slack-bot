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
import           Data.Text.Lazy              as T hiding (unpack)
import           Data.Text.Lazy.Encoding
import           Data.Yaml
import Data.Function
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
import System.Eval.Haskell


tryCommit = True


commands ∷ [(ByteString, AppSettings → HookData → IO Text)]
commands =
  [ ("bashthis:", addNew)
  , ("evaluate:", evaluateCode)
  ]


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
                               } deriving (Show)


truncateCommand command = C.dropWhile isSpace . C.drop (C.length command)


evaluateHaskell = fmap (maybe "Evaluation failed" show) . flip unsafeEval ["Prelude"]


evaluableLanguages =
  [ ("hs", evaluateHaskell)
  , ("haskell", evaluateHaskell)
  ]


addNew
  (AppSettings { username = user, password = passwd, uri = uri, minQuoteLength = mql })
  (HookData { command = cmd, text = text })
  = if lengthVerifier quote
    then browse $ do
      setAuthorityGen (\_ _ → return $ return (user, passwd))
      setAllowBasicAuth True
      request req
      liftIO $ putStrLn "success"
      return "Yeey, new quotes!!! Thank you 😃"
    else
      return $ "Your quote is too short, the bash will reject it 😐. "
        <> maybe "" (\req -> "Just make it like at least " <> T.pack (show (req - C.length quote)) <> " characters longer.") mql
    where
      quote = truncateCommand cmd text
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


evaluateCode
  _
  (HookData { command = cmd, text = text })
  =
    case C.unwords $ truncateCommand cmd text of
      t@(lang:r) ->
        let
          code = truncateCommand lang t
        in
          maybe "Unknown Language" (fmap (wrapCode . T.pack) . (code =<<)) $ lookup lang evaluableLanguages
      _ -> return "You need provide a language"
    where
      wrapCode c = "`" <> c <> "`"


instance FromJSON AppSettings where
  parseJSON (Object o) = AppSettings
    <$> o .: "port"
    ⊛ (fmap $ fmap C.pack) (o .:? "token")
    ⊛ o .: "password"
    ⊛ o .: "username"
    ⊛ fmap (fromJust ∘ parseURI) (o .: "target")
    ⊛ o .:? "min_quote_length"


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
              print postData
              action settings postData ≫= respondSuccess
        else respondFail

  where
    verifier = maybe (const True) (≡) expectedToken
    respondSuccess message =
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ encodeUtf8 $ "{\"text\":\"" <> message <> "\"}"
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
