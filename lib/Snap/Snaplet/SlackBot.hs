{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.SlackBot
  ( initBot
  , SlackBot
  ) where


import           Control.Applicative    ((<|>))
import           Control.Category       ((>>>))
import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class
import           Data.Aeson             as JSON
import           Data.ByteString
import qualified Data.ByteString.Char8  as C hiding (putStrLn, unlines)
import           Data.Char
import qualified Data.Configurator      as Cf
import qualified Data.Map.Lazy          as Map
import           Data.Maybe
import           Data.Monoid            ((<>))
import           Data.Text              as T
import           Network.Browser
import qualified Network.HTTP.Base      as HTTPB
import           Network.URI
import           Safe
import           Snap
import           Snap.Snaplet
import           System.FilePath        ((</>))
import           System.IO              as SIO (hPutStrLn, stderr)



data HookData = HookData { hookDataToken :: ByteString
                         , command       :: ByteString
                         , text          :: ByteString
                         } deriving (Show)


newtype SlackResponse = SlackResponse Text deriving (Show)


data SlackBot = SlackBot


instance ToJSON SlackResponse where
  toJSON (SlackResponse text) = object [ "text" .= text ]


logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . SIO.hPutStrLn stderr


logShow :: (MonadIO m, Show s) => s -> m ()
logShow = logMsg . show


commands :: Map.Map ByteString (HookData -> Handler b SlackBot Text)
commands = Map.fromList
  [ ("bashthis:", addNew)
  ]


require cfg = liftIO . Cf.require cfg
lookupcfg cfg = liftIO . Cf.lookup cfg


initBot :: SnapletInit b SlackBot
initBot = makeSnaplet "slack-bot" "react to slack messages" Nothing $ do
    addRoutes [("", handler)]
    return SlackBot


truncateCommand :: ByteString -> ByteString -> ByteString
truncateCommand command = C.dropWhile isSpace . C.drop (C.length command)


handler :: Handler b SlackBot ()
handler = do
  expectedToken <- getSnapletUserConfig >>= flip lookupcfg "bash.token"
  (parseData <$> getPostParams) >>= \case
    Nothing -> respondFail
    Just postData@(HookData { hookDataToken = token, command = command, text = text }) ->
      case expectedToken of
        Just t | t /= token -> logMsg "Token did not match" >> respondFail
        _ ->
          case Map.lookup command commands of
            Nothing -> respondFail
            Just action -> action postData >>= respondSuccess

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



addNew :: HookData -> Handler b SlackBot Text
addNew (HookData { command, text }) = do
    config <- getSnapletUserConfig
    username <- require config "user.username"
    password <- require config "user.password"
    !uri <- fromMaybe (error "bash url must be a valid url") . parseURI <$> require config "bash.target"
    minQuoteLength <- lookupcfg config "bash.min_quote_length"
    let quote = truncateCommand command text
        lengthVerifier =
          maybe
            (const True)
            (\a b -> a <= C.length b)
            minQuoteLength
        req = formToRequest $
            Form
              HTTPB.POST
              uri
              [ ("rash_quote", C.unpack quote)
              , ("submit", "Add Quote")
              ]
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


parseData :: Map.Map ByteString [ByteString] -> Maybe HookData
parseData params = HookData
  <$> (Map.lookup "token" params >>= headMay)
  <*> (Map.lookup "trigger_word" params >>= headMay)
  <*> (Map.lookup "text" params >>= headMay)
