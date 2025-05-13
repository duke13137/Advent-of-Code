{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021               #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Day01.Main (main) where

import Debug.Breakpoint
import Rapid

import Imports
import Optics

import Network.Wai.Handler.Warp qualified as Http
import Test.Tasty.Wai as Test
import Web.Twain hiding (get)
import Web.Twain qualified as Http

import CustomHsx (hsx)
import IHP.HSX.ConvertibleStrings ()
import IHP.HSX.ToHtml ()
import Lucid (Html, renderBS)

import Colog

import Data.Text qualified as Text

main :: IO ()
main = do
  -- server
  reload

-- $> reload
reload :: IO ()
reload =
  rapid 0 \r ->
    restart r "http" server

-- $> tasty tests
tests ::TestTree
tests = testGroup "Tasty.Wai Tests"
  [ testWai app "Hello World" do
      resp <- Test.get "/"
      assertStatus 200 resp
      assertBody "<h1>hello, world!</h1>" resp
  , testWai app "Not found" do
      resp <- Test.get "/notfound"
      assertStatus' status404 resp
      assertBodyContains "Not found" resp
  ]

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
  logDebug $ "name: " <>  name
  -- breakpointM
  lift $ render [hsx| <h1 id="hello">Hello, {name}!</h1> |]

missing :: ResponderM a
missing = send $ text "Not found..."
