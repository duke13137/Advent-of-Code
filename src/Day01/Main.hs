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
module Day01.Main (main) where

import Debug.Breakpoint
import Rapid

import Imports
import Optics

import Network.Wai.Handler.Warp qualified as Http
import Web.Twain hiding (get)
import Web.Twain qualified as Http

import CustomHsx (hsx)
import IHP.HSX.ConvertibleStrings ()
import IHP.HSX.ToHtml ()
import Lucid
import Witch

main :: IO ()
main = reload -- webserver

-- $> reload
reload :: IO ()
reload =
  rapid 0 \r ->
    restart r "webserver" webserver

webserver :: IO ()
webserver =
  Http.runEnv 8080 $ foldr ($) (notFound missing) routes

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
index = render [hsx| <h1>Hello World</h1> |]

echoName :: ResponderM a
echoName = do
  name <- param @Text "name"
  -- breakpointM
  render [hsx| <h1 id="hello">Hello, {name}!</h1> |]

missing :: ResponderM a
missing = send $ json @Text "Not found..."
