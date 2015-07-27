{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Arrow               hiding (app)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Unicode
import           Data.ByteString.Char8       as C hiding (putStrLn, unlines)
import qualified Data.ByteString.Lazy        as BL
import           Data.Char
import           Data.Maybe
import           Data.Monoid.Unicode
import           Data.Text.Lazy              as T hiding (unpack)
import           Data.Text.Lazy.Encoding
import           Data.Yaml
import           Network.Browser
import           Network.HTTP                hiding (password, port)
import           Network.HTTP.Types          hiding (POST)
import           Network.URI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           Prelude                     as P hiding (log)
import           Prelude.Unicode
import           System.Environment
import           System.Exit
import qualified System.IO                   as SIO
import           System.Process
import           Text.Printf


log ∷ String → IO ()
log = SIO.hPutStrLn SIO.stderr


logShow ∷ Show ς ⇒ ς → IO ()
logShow = log . show


commands ∷ [(ByteString, AppSettings → HookData → IO Text)]
commands =
  [ ("bashthis:", addNew)
  , ("evaluate:", evaluateCode)
  ]


evaluableLanguages ∷ [(ByteString, AppSettings → String → IO String)]
evaluableLanguages =
  [ ("ruby", evaluateRuby )
  , ("rb", evaluateRuby)
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


evaluateCode ∷ AppSettings → HookData → IO Text
evaluateCode
  settings
  (HookData { command, text })
  =
    case C.words truncated of
      (language:_:_) ->
        maybe
          (return $ "Sorry, I could not find language '" ⊕ (decodeUtf8 (BL.fromStrict language)) ⊕ "'")
          (\f -> T.pack <$> f settings (C.unpack $ truncateCommand language truncated))
          $ lookup language evaluableLanguages
      [] -> return $ "What language should I interpret? The sytax is '" ⊕ textCommand ⊕ " <language> <code>.'"
      _ -> return "You didn't give me any code to execute"
  where
    truncated = truncateCommand command text
    textCommand = decodeUtf8 $ BL.fromStrict command


evaluateRuby ∷ AppSettings → String → IO String
evaluateRuby settings code = do
  executed <- readProcessWithExitCode
                "ruby"
                (P.map ("-e " ⊕) $ P.lines code)
                ""
  case executed of
    (ExitSuccess, out, err) → return out
    (ExitFailure nr, out, err) → do
      log $ "A call to the ruby command failed with code " ⊕ show nr
      log $ "stdout: " ⊕ out
      log $ "stderr: " ⊕ err
      return $ "Sorry, but calling 'ruby' with your input failed '" ⊕ err ⊕ "'"



truncateCommand ∷ ByteString → ByteString → ByteString
truncateCommand command = C.dropWhile isSpace ∘ C.drop (C.length command)


addNew ∷ AppSettings → HookData → IO Text
addNew
  (AppSettings { username, password, uri, minQuoteLength })
  (HookData { command, text })
  =
  if lengthVerifier quote
    then browse $ do
      setAuthorityGen (\_ _ → return $ return (username, password))
      setAllowBasicAuth True
      _ ← request req
      liftIO $ log "success"
      return "Yeey, new quotes!!! Thank you 😃"
    else
      return $ "Your quote is too short, the bash will reject it 😐. "
        ⊕ maybe "" (\required →
            "Just make it like at least "
            ⊕ T.pack (show (required - C.length quote))
            ⊕ " characters longer."
          ) minQuoteLength
    where
      quote = truncateCommand command text
      lengthVerifier = maybe (const True) (\a b → a ≤ C.length b) minQuoteLength
      req = formToRequest body
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
  parseJSON _ = mzero


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
              logShow postData
              action settings postData ≫= respondSuccess
        else log "Token did not match" ≫ respondFail

  where
    verifier = maybe (const True) (≡) expectedToken
    respondSuccess message =
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ encodeUtf8 $ "{\"text\":\"" ⊕ message ⊕ "\"}"
    respondFail =
      respond $ responseLBS badRequest400 [] $ encodeUtf8 "{\"text\": \"Sorry, something went wrong 😐\"}"


main ∷ IO ()
main = do
  log "Bash Slack Bot  -- Version: 0.1.0.0"
  [settingsFile] ← getArgs
  sf ← decodeFile settingsFile
  case sf of
    Nothing → log "could not read settings"
    Just conf → do
      log "Starting server!"
      log
        $ printf
          "Port: %d   Token: %s   Target URI: %s   with Username: %s   and Password: %s"
          (port conf)
          (maybe "-" (uncurry (⧺) ∘ second (flip P.replicate '*' ∘ P.length) ∘ P.splitAt 6 ∘ C.unpack) $ appSettingsToken conf)
          (show $ uri conf)
          (username conf)
          (P.replicate (P.length $ password conf) '*')

      run (port conf) (app conf)
