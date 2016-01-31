{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Snap.Snaplet.SlackBot 
  ( initBot
  ) where


import qualified Data.Configurator as Cf
import Safe
import Data.Aeson as JSON
import Data.ByteString
import Snap
import Snap.Snaplet
import Data.Text as T
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8    as C hiding (putStrLn, unlines)
import Data.Char
import           Network.Browser
import Control.Monad ((>=>))
import qualified Data.Map.Lazy as Map
import System.IO as SIO (stderr, hPutStrLn)
import Control.Monad.IO.Class
import Control.Category ((>>>))
import Network.URI
import qualified Network.HTTP.Base as HTTPB
import Control.Applicative ((<|>))
import System.FilePath ((</>))



data HookData = HookData { hookDataToken :: ByteString
                         , command       :: ByteString
                         , text          :: ByteString
                         } deriving (Show)


data BotSettings = BotSettings { port             :: Int
                               , appSettingsToken :: Maybe ByteString
                               , password         :: String
                               , username         :: String
                               , uri              :: URI
                               , minQuoteLength   :: Maybe Int
                               } deriving (Show)


newtype SlackResponse = SlackResponse Text deriving (Show)


data SlackBot = SlackBot


instance ToJSON SlackResponse where
  toJSON (SlackResponse text) = object [ "text" .= text ]


logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . SIO.hPutStrLn stderr


logShow :: (MonadIO m, Show s) => s -> m ()
logShow = logMsg . show


commands :: Map.Map ByteString (BotSettings -> HookData -> Handler b SlackBot Text)
commands = Map.fromList
  [ ("bashthis:", addNew)
  ]


initBot :: Maybe FilePath -> SnapletInit b SlackBot
initBot path = 
  makeSnaplet 
    "slack-bot"
    "react to slack messages" 
    (return <$> (path <|> Just "slack-bot")) 
    $ do
      cfg <- readSlackConfig $ fromMaybe "slack-bot" path </> "config.cfg"
      addRoutes [("", handler cfg)]
      return SlackBot


readSlackConfig :: MonadIO m => FilePath -> m BotSettings
readSlackConfig path = liftIO $ do
  sf <- (Cf.load [Cf.Required path])
  BotSettings
    <$> Cf.require sf "server.port"
    <*> Cf.lookup sf "bash.token"
    <*> Cf.require sf "user.password"
    <*> Cf.require sf "user.username"
    <*> (fromMaybe (error "bash url must be a valid url") . parseURI <$> Cf.require sf "bash.target")
    <*> Cf.lookup sf  "bash.min_quote_length"

 
truncateCommand :: ByteString -> ByteString -> ByteString
truncateCommand command = C.dropWhile isSpace . C.drop (C.length command)


handler :: BotSettings -> Handler b SlackBot ()
handler settings@(BotSettings { appSettingsToken = expectedToken }) =
  (parseData <$> getPostParams) >>= \case 
    Nothing -> respondFail
    Just postData@(HookData { hookDataToken = token, command = command, text = text }) ->
      if maybe (const True) (==) expectedToken token
        then
          case Map.lookup command commands of
            Nothing -> respondFail
            Just action -> do
              logMsg $ "received new request for command " <> C.unpack command
              action settings postData >>= respondSuccess
        else logMsg "Token did not match" >> respondFail

  where
    respondSuccess message = do
        logShow json
        modifyResponse 
          $ setResponseCode 200
          >>> setContentType "application/json"
        writeLBS (JSON.encode json)
      where
        json = SlackResponse message
    respondFail = do
      modifyResponse (setResponseCode 400)
      writeLBS $ JSON.encode $ SlackResponse "Sorry, something went wrong 😐"



addNew :: BotSettings -> HookData -> Handler b SlackBot Text
addNew 
  (BotSettings { username, password, uri, minQuoteLength })
  (HookData { command, text }) =
    if lengthVerifier quote
      then liftIO $ browse $ do
        setAuthorityGen (\_ _ -> return $ return (username, password))
        setAllowBasicAuth True
        _ <- request req
        logMsg "success"
        return "Yeey, new quotes!!! Thank you 😃"
      else
        return $ "Your quote is too short, the bash will reject it 😐. "
          <> maybe "" (\required ->
              "Just make it like at least "
              <> T.pack (show (required - C.length quote))
              <> " characters longer."
            ) minQuoteLength
    where
      quote = truncateCommand command text
      lengthVerifier = maybe (const True) (\a b -> a <= C.length b) minQuoteLength
      req = formToRequest body
      body =
        Form
          HTTPB.POST
          uri
          [ ("rash_quote", C.unpack quote)
          , ("submit", "Add Quote")
          ]


parseData :: Map.Map ByteString [ByteString] -> Maybe HookData
parseData params = HookData
  <$> (Map.lookup "token" params >>= headMay)
  <*> (Map.lookup "trigger_word" params >>= headMay)
  <*> (Map.lookup "text" params >>= headMay)



