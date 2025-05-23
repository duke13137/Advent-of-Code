{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Main (main, app, server) where

import Debug.Breakpoint

import Optics
import Prelude

import Colog
import Html
import Network.Wai.Handler.Warp qualified as Http
import Web.Twain as Http

import Database

main :: IO ()
main = server

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

server :: IO ()
server = Http.runEnv 8080 app

app :: Application
app = foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ Http.get "/" index
  , Http.get "/echo/:name" echoName
  ]

template :: Html () -> Html ()
template body = [hsx|
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>My Simple HTML Page</title>
      <script src="https://unpkg.com/htmx.org@2.0.4"/>
    </head>
    <body hx-boost="true">
      {body}
    </body>
  </html>
|]

render :: Html () -> ResponderM a
render = send . html . renderBS . template

index :: ResponderM a
index = send $ html "<h1>hello, world!</h1>"

echoName :: ResponderM a
echoName = logger do
  name <- lift $ param @Text "name"
  logInfo $ "name: " <>  name
  -- breakpointM
  lift $ render [hsx| <h1 id="hello">Hello, {name}!</h1> |]

missing :: ResponderM a
missing = send $ text "Not found..."
