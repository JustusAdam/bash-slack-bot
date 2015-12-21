{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Arrow               hiding (app)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Unicode
import           Data.Aeson                  as JSON
import           Data.ByteString.Char8       as C hiding (putStrLn, unlines)
import qualified Data.ByteString.Lazy        as BL
import           Data.Char
import           Data.Configurator           as Cf
import qualified Data.List                   as L
import           Data.Maybe
import           Data.Monoid.Unicode
import           Data.Text.Lazy              as T hiding (unpack)
import           Data.Text.Lazy.Encoding
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
  [ ("ruby", evaluateRuby)
  , ("rb", evaluateRuby)
  , ("py", evaluatePython)
  , ("python", evaluatePython)
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


data SlackResponse = SlackResponse Text deriving (Show)


instance ToJSON SlackResponse where
  toJSON (SlackResponse text) = object [ "text" .= text ]


evaluateCode ∷ AppSettings → HookData → IO Text
evaluateCode
  settings
  (HookData { command, text })
  =
    case C.words truncated of
      (language:_:_) ->
        maybe
          (return $ "Sorry, I could not find language '" ⊕ decodeUtf8 (BL.fromStrict language) ⊕ "'")
          (\f -> T.pack <$> f settings (C.unpack $ truncateCommand language truncated))
          $ P.lookup language evaluableLanguages
      [] -> return $ "What language should I interpret? The sytax is '" ⊕ textCommand ⊕ " <language> <code>.'"
      _ -> return "You didn't give me any code to execute"
  where
    truncated = truncateCommand command text
    textCommand = decodeUtf8 $ BL.fromStrict command


evaluateCodeForeighnCall ∷ (a → b → IO (ExitCode, String, String)) → a → b → IO String
evaluateCodeForeighnCall action settings code = do
  executed <- action settings code
  case executed of
    (ExitSuccess, out, err) → do
      log $ "succeeded with '" ⊕ out ⊕ "'"
      return out
    (ExitFailure nr, out, err) → do
      log $ "A call to the command failed with code " ⊕ show nr
      log $ "stdout: " ⊕ out
      log $ "stderr: " ⊕ err
      return $ "Sorry, but calling with your input failed '" ⊕ err ⊕ "'"


evaluateRuby ∷ AppSettings → String → IO String
evaluateRuby = evaluateCodeForeighnCall rubyCommand


rubyCommand ∷ AppSettings → String → IO (ExitCode, String, String)
rubyCommand settings code = do
  log $ "evaluating ruby command: '" ⊕ code ⊕ "'"
  readProcessWithExitCode "ruby" args ""
  where
    args = "-e":L.intersperse "-e" (P.lines code)


evaluatePython ∷ AppSettings → String → IO String
evaluatePython = evaluateCodeForeighnCall pythonCommand


pythonCommand ∷ AppSettings → String → IO (ExitCode, String, String)
pythonCommand settings code = do
  log $ "evaluating python command: '" ⊕ code ⊕ "'"
  readProcessWithExitCode "python3" ["-c", code] ""


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


parseData ∷ [(ByteString, ByteString)] → Maybe HookData
parseData params = HookData
  <$> P.lookup "token" params
  ⊛ P.lookup "trigger_word" params
  ⊛ P.lookup "text" params


app ∷ AppSettings → Application
app settings@(AppSettings { appSettingsToken = expectedToken }) req respond = do
  mpostData ← parseData ∘ fst <$> parseRequestBody lbsBackEnd req

  case mpostData of
    Nothing → respondFail
    Just postData@(HookData { hookDataToken = token, command = command, text = text }) →
      if verifier token
        then
          case P.lookup command commands of
            Nothing → respondFail
            Just action → do
              log $ "received new request for command " ⊕ C.unpack command
              action settings postData ≫= respondSuccess
        else log "Token did not match" ≫ respondFail

  where
    verifier = maybe (const True) (≡) expectedToken
    respondSuccess message = do
      logShow json
      respond $ responseLBS status200 [("Content-Type", "application/json")] $ JSON.encode json
      where
        json = SlackResponse message
    respondFail =
      respond $ responseLBS badRequest400 [] $ JSON.encode $ SlackResponse "Sorry, something went wrong 😐"


main ∷ IO ()
main = do
  log "Bash Slack Bot  -- Version: 0.1.0.0"
  [settingsFile] ← getArgs
  sf ← load [Required settingsFile]

  conf <- AppSettings
            <$> require sf "server.port"
            <*> Cf.lookup sf "bash.token"
            <*> require sf "user.password"
            <*> require sf "user.username"
            <*> (fromMaybe (error "bash url must be a valid url") . parseURI <$> require sf "bash.target")
            <*> Cf.lookup sf  "bash.min_quote_length"

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
