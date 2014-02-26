{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module App (runApp) where

import Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <p> Hello World!
    <ul>
        $forall (resource,name) <- zip [Page1R,Page2R] ["page 1","page 2"]
            <li><a href=@{resource}>Go to #{name}!
    |]

getPage1R :: Handler Html
getPage1R = defaultLayout [whamlet|
    <p> Welcome to page 1!
    <a href=@{HomeR}>Go home!
    |]

getPage2R :: Handler Html
getPage2R = defaultLayout [whamlet|
    <p>Welcome to page 2!
    <a href=@{HomeR}>Go home!
    |]


runApp :: Int -> IO ()
runApp port = warp port Links
