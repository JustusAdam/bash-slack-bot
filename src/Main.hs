module Main where


import           Snap
import           Snap.Snaplet.SlackBot


main = do
    cfg <- commandLineConfig emptyConfig
    serveSnaplet cfg (initBot Nothing)
