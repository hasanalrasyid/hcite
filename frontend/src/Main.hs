{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe,fromJust)
import           Data.Monoid ((<>))
import           Data.FileEmbed

import Data.Witherable

import Common

import Control.Monad.Fix
import Reflex.Dom.Xhr

import Language.Javascript.JSaddle.Types
import Control.Monad.IO.Class

main :: IO ()
main = mainWidgetWithHead headElement body

headElement :: MonadWidget t m => m ()
headElement = do
  {-
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
-}
  el "title" $ text "Main Title"
  stylesheet "inc/css/font-awesome.min.css"
  stylesheet "inc/css/OpenSans.css"
  stylesheet "inc/css/bulma.min.css"
  stylesheet "inc/css/forum.css"
  where
    stylesheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

  {-
main1 = mainWidgetWithCss css body
   where css = $(embedFile "css/tab.css")
-}

-- | Enumeration to track type of page to display
data Page = PageData | PageError
   deriving Eq

body' :: MonadWidget t m => m ()
body' =
  elClass "nav" "navbar is-white topNav" $ do
    elClass "div" "container" $ do
      elClass "div" "navbar-brand" $ do
        elClass "a" "navbar-item" $ -- href="../">
          elAttr "img" (("src" =: "inc/img/bulma.png") <> ("width" =: "112") <> ("height"=:"28")) blank
        elClass "div" "navbar-burger burger" blank -- data-target="topNav">
--  <span></span>
--  <span></span>
--  <span></span>
--      </div>
--    </div>
      elClass "div" "navbar-menu" $ do -- id="topNav" class="navbar-menu">
        elClass "div" "navbar-start" $ do
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "cover.html"          )) $ text "Home"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "landing.html"        )) $ text "Landing"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "blog.html"           )) $ text "Blog"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "instaAlbum.html"     )) $ text "Album"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "kanban[search].html" )) $ text "Kanban"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "search.html"         )) $ text "Search"
          elAttr "a" ( ("class" =: "navbar-item") <> ( "href" =: "tabs.html"           )) $ text "Tabs"
--      </div>

        elClass "div" "navbar-end" $ do
          elClass "div" "navbar-item" $ do
            elClass "div" "field is-grouped"$ do
              elClass "p" "control" $ do
                elClass "a" "button is-small" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user-plus" blank
                  el "span" $ text "Register"
--            </p>
              elClass "p" "control" $ do
                elClass "a" "button is-small is-info is-outlined" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user" blank
                  el "span" $ text "Login"
                    {-
--            </p>
--          </div>
--        </div>
--      </div>
--    </div>
--  </div>
  </nav>
    -}

  {-
  <nav class="navbar is-white">
  <div class="container">
  <div class="navbar-menu">
  <div class="navbar-start">
  <a class="navbar-item is-active" href="#">Popular</a>
  <a class="navbar-item" href="#">Recent</a>
  <a class="navbar-item" href="#">Rising</a>
  </div>
  <div class="navbar-end">
  <div class="navbar-item">
  <input class="input" type="search" placeholder="Search forum...">
  </div>
  </div>
  </div>
  </div>
  </nav>
  <section class="container">
  <div class="columns">
  <div class="column is-3">
  <a class="button is-primary is-block is-alt is-large" href="#">New Post</a>
  <aside class="menu">
  <p class="menu-label">
  Tags
  </p>
  <ul class="menu-list">
  <li><span class="tag is-primary is-medium ">Dashboard</span></li>
  <li><span class="tag is-link is-medium ">Customers</span></li>
  <li><span class="tag is-light is-danger is-medium ">Authentication</span></li>
  <li><span class="tag is-dark is-medium ">Payments</span></li>
  <li><span class="tag is-success is-medium ">Transfers</span></li>
  <li><span class="tag is-warning is-medium ">Balance</span></li>
  <li><span class="tag is-medium ">Question</span></li>
  </ul>
  </aside>
  </div>
  <div class="column is-9">
  <div class="box content">
  <article class="post">
  <h4>Bulma: How do you center a button in a box?</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@jsmith</a> replied 34 minutes ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 1</span>
  </div>
  </div>
  </article>
  <article class="post">
  <h4>How can I make a bulma button go full width?</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@red</a> replied 40 minutes ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 0</span>
  </div>
  </div>
  </article>
  <article class="post">
  <h4>TypeError: Data must be a string or a buffer when trying touse vue-bulma-tabs</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@jsmith</a> replied 53 minutes ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 13</span>
  </div>
  </div>
  </article>
  <article class="post">
  <h4>How to vertically center elements in Bulma?</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@brown</a> replied 3 hours ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
  </div>
  </div>
  </article>
  <article class="post">
  <h4>I'm trying to use hamburger menu on bulma css, but it doesn't work. What is wrong?</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@hamburgler</a> replied 5 hours ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
  </div>
  </div>
  </article>
  <article class="post">
  <h4>How to make tiles wrap with Bulma CSS?</h4>
  <div class="media">
  <div class="media-left">
  <p class="image is-32x32">
  <img src="http://bulma.io/images/placeholders/128x128.png">
  </p>
  </div>
  <div class="media-content">
  <div class="content">
  <p>
  <a href="#">@rapper</a> replied 3 hours ago &nbsp;
  <span class="tag">Question</span>
  </p>
  </div>
  </div>
  <div class="media-right">
  <span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
  </div>
  </div>
  </article>
  </div>
  </div>
  </div>
  </section>
  <footer class="footer">
  <div class="container">
  <div class="content has-text-centered">
  <div class="columns is-mobile is-centered">
  <div class="field is-grouped is-grouped-multiline">
  <div class="control">
  <div class="tags has-addons">
  <a class="tag is-link" href="https://github.com/BulmaTemplates/bulma-templates">Bulma Templates</a>
  <span class="tag is-light">Daniel Supernault</span>
  </div>
  </div>
  <div class="control">
  <div class="tags has-addons">
  <a class="tag is-link">The source code is licensed</a>
  <span class="tag is-light">MIT &nbsp;<i class="fa fa-github"></i></span>
  </div>
  </div>
  </div>
  </div>
  </div>
  </div>
  </footer>
  <script async type="text/javascript" src="../js/bulma.js"></script>
  </body>
-}
  {-
<body>
<nav class="navbar is-white topNav">
<div class="container">
<div class="navbar-brand">
<a class="navbar-item" href="../">
<img src="../images/bulma.png" width="112" height="28">
</a>
<div class="navbar-burger burger" data-target="topNav">
<span></span>
<span></span>
<span></span>
</div>
</div>
<div id="topNav" class="navbar-menu">
<div class="navbar-start">
<a class="navbar-item" href="cover.html">Home</a>
<a class="navbar-item" href="landing.html">Landing</a>
<a class="navbar-item" href="blog.html">Blog</a>
<a class="navbar-item" href="instaAlbum.html">Album</a>
<a class="navbar-item" href="kanban[search].html">Kanban</a>
<a class="navbar-item" href="search.html">Search</a>
<a class="navbar-item" href="tabs.html">Tabs</a>
</div>
<div class="navbar-end">
<div class="navbar-item">
<div class="field is-grouped">
<p class="control">
<a class="button is-small">
<span class="icon">
<i class="fa fa-user-plus"></i>
</span>
<span>
Register
</span>
</a>
</p>
<p class="control">
<a class="button is-small is-info is-outlined">
<span class="icon">
<i class="fa fa-user"></i>
</span>
<span>Login</span>
</a>
</p>
</div>
</div>
</div>
</div>
</div>
</nav>
<nav class="navbar is-white">
<div class="container">
<div class="navbar-menu">
<div class="navbar-start">
<a class="navbar-item is-active" href="#">Popular</a>
<a class="navbar-item" href="#">Recent</a>
<a class="navbar-item" href="#">Rising</a>
</div>
<div class="navbar-end">
<div class="navbar-item">
<input class="input" type="search" placeholder="Search forum...">
</div>
</div>
</div>
</div>
</nav>
<section class="container">
<div class="columns">
<div class="column is-3">
<a class="button is-primary is-block is-alt is-large" href="#">New Post</a>
<aside class="menu">
<p class="menu-label">
Tags
</p>
<ul class="menu-list">
<li><span class="tag is-primary is-medium ">Dashboard</span></li>
<li><span class="tag is-link is-medium ">Customers</span></li>
<li><span class="tag is-light is-danger is-medium ">Authentication</span></li>
<li><span class="tag is-dark is-medium ">Payments</span></li>
<li><span class="tag is-success is-medium ">Transfers</span></li>
<li><span class="tag is-warning is-medium ">Balance</span></li>
<li><span class="tag is-medium ">Question</span></li>
</ul>
</aside>
</div>
<div class="column is-9">
<div class="box content">
<article class="post">
<h4>Bulma: How do you center a button in a box?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@jsmith</a> replied 34 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 1</span>
</div>
</div>
</article>
<article class="post">
<h4>How can I make a bulma button go full width?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@red</a> replied 40 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 0</span>
</div>
</div>
</article>
<article class="post">
<h4>TypeError: Data must be a string or a buffer when trying touse vue-bulma-tabs</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@jsmith</a> replied 53 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 13</span>
</div>
</div>
</article>
<article class="post">
<h4>How to vertically center elements in Bulma?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@brown</a> replied 3 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
<article class="post">
<h4>I'm trying to use hamburger menu on bulma css, but it doesn't work. What is wrong?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@hamburgler</a> replied 5 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
<article class="post">
<h4>How to make tiles wrap with Bulma CSS?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@rapper</a> replied 3 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
</div>
</div>
</div>
</section>
<footer class="footer">
<div class="container">
<div class="content has-text-centered">
<div class="columns is-mobile is-centered">
<div class="field is-grouped is-grouped-multiline">
<div class="control">
<div class="tags has-addons">
<a class="tag is-link" href="https://github.com/BulmaTemplates/bulma-templates">Bulma Templates</a>
<span class="tag is-light">Daniel Supernault</span>
</div>
</div>
<div class="control">
<div class="tags has-addons">
<a class="tag is-link">The source code is licensed</a>
<span class="tag is-light">MIT &nbsp;<i class="fa fa-github"></i></span>
</div>
</div>
</div>
</div>
</div>
</div>
</footer>
<script async type="text/javascript" src="../js/bulma.js"></script>
</body>
-}
  {-
-- | Create the HTML body
body :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadJSM (Performable m), PerformEvent t m, HasJSContext (Performable m), TriggerEvent t m, FromJSON a ) => m ()
-}
body :: MonadWidget t m => m ()
body  = el "div" $ do
  body'
  el "h2" $ text "Swiss Weather Data (Tab display)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  el "p" blank
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- getAndDecode $ buildReq <$> evCode
  -- Check on HTML response code and remember state.
--  let (evOk, evErr) = checkXhrRsp' evRsp
--  dynPage <- foldDyn ($) PageData $ leftmost [const PageData <$ evOk, const PageError <$ evErr]
  dynPage <- foldDyn ($) PageData $ leftmost [const PageData <$ evRsp]
  -- Create the 2 pages
  pageData' evRsp dynPage
  pageErr   evRsp dynPage
  return ()

pageData' :: (PostBuild t m, DomBuilder t m, MonadHold t m) => Event t (Maybe SmnRecord) -> Dynamic t Page -> m ()
pageData' evSmnRec' dynPage = do
  evSmnRec :: (Event t SmnRecord) <- return $ fmap fromJust evSmnRec'
  let evSmnStat = fmapMaybe smnStation evSmnRec
  let dynAttr = visible <$> dynPage <*> pure PageData
  elDynAttr "div" dynAttr $ do
    tabStat evSmnStat
    tabMeteo evSmnRec
--    tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat

-- | Display the meteo data in a tabbed display
pageData :: (PostBuild t m, DomBuilder t m) => Event t XhrResponse -> Dynamic t Page -> m ()
pageData evOk dynPage = do
  evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evOk
  let evSmnStat = fmapMaybe smnStation evSmnRec
  let dynAttr = visible <$> dynPage <*> pure PageData
  elDynAttr "div" dynAttr $ text "PageData"
--    tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat

-- | Display the error page
--pageErr :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m ()
pageErr evErr dynPage = do
  let dynAttr = visible <$> dynPage <*> pure PageError
  elDynAttr "div" dynAttr $ do
     el "h3" $ text "Error"
     dynText =<< holdDyn "" ( const "PageError" <$> evErr)

-- | Split up good and bad response events
checkXhrRsp :: (Filterable f, FunctorMaybe f) => f XhrResponse -> (f XhrResponse, f XhrResponse)
checkXhrRsp evRsp = (evOk, evErr)
  where
    evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
    evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp

-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Eq p => p -> p -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where
    choose True  t _ = t
    choose False _ f = f

  {-
getEvRsp :: Event t T.Text -> m (Event t XhrResponse)
getEvRsp evCode = performRequestAsync $ buildReq <$> evCode
-}

--buildReq :: T.Text -> XhrRequest ()
--buildReq code = XhrRequest "GET" (urlDataStat code) def
buildReq :: T.Text -> T.Text
buildReq code = urlDataStat code

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

-- | Create a tabbed display
--tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evMeteo evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabMeteo evMeteo))]

-- | Create the DOM elements for the Station tab
--tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat evStat = do
  dispStatField "Code" staCode evStat
  dispStatField "Name" staName evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Meteo data tab
--tabMeteo :: MonadWidget t m => Event t SmnRecord -> m ()
tabMeteo evMeteo = do
  dispMeteoField "Date/Time" (tShow . smnDateTime) evMeteo
  dispMeteoField "Temperature" smnTemperature evMeteo
  dispMeteoField "Sunshine" smnSunshine evMeteo
  dispMeteoField "Precipitation" smnPrecipitation evMeteo
  dispMeteoField "Wind Direction" smnWindDirection evMeteo
  dispMeteoField "Wind Speed" smnWindSpeed evMeteo
  return ()

-- | Display a single field from the SmnStation record
--dispStatField :: MonadWidget t m => T.Text -> (SmnStation -> T.Text) -> Event t SmnStation -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- | Display a single field from the SmnRecord record
--dispMeteoField :: MonadWidget t m => T.Text -> (SmnRecord -> T.Text) -> Event t SmnRecord -> m ()
dispMeteoField label rend evRec = do
--  el "br"blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evRec)
  return ()

-- | Small helper function to convert showable values wrapped in Maybe to T.Text.
-- You should use the test-show library from Hackage!!
tShow :: Show a => Maybe a -> T.Text
tShow Nothing = ""
tShow (Just x) = (T.pack . show) x

  {-
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Forum - Free Bulma template</title>
<link rel="shortcut icon" href="../images/favicon.png" type="image/x-icon">
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css">
<link href="https://fonts.googleapis.com/css?family=Open+Sans:300,400,700" rel="stylesheet">
<!-- Bulma Version 0.8.x-->
<link rel="stylesheet" href="https://unpkg.com/bulma@0.8.0/css/bulma.min.css" />
<link rel="stylesheet" type="text/css" href="../css/forum.css">
</head>


<body>
<nav class="navbar is-white topNav">
<div class="container">
<div class="navbar-brand">
<a class="navbar-item" href="../">
<img src="../images/bulma.png" width="112" height="28">
</a>
<div class="navbar-burger burger" data-target="topNav">
<span></span>
<span></span>
<span></span>
</div>
</div>
<div id="topNav" class="navbar-menu">
<div class="navbar-start">
<a class="navbar-item" href="cover.html">Home</a>
<a class="navbar-item" href="landing.html">Landing</a>
<a class="navbar-item" href="blog.html">Blog</a>
<a class="navbar-item" href="instaAlbum.html">Album</a>
<a class="navbar-item" href="kanban[search].html">Kanban</a>
<a class="navbar-item" href="search.html">Search</a>
<a class="navbar-item" href="tabs.html">Tabs</a>
</div>
<div class="navbar-end">
<div class="navbar-item">
<div class="field is-grouped">
<p class="control">
<a class="button is-small">
<span class="icon">
<i class="fa fa-user-plus"></i>
</span>
<span>
Register
</span>
</a>
</p>
<p class="control">
<a class="button is-small is-info is-outlined">
<span class="icon">
<i class="fa fa-user"></i>
</span>
<span>Login</span>
</a>
</p>
</div>
</div>
</div>
</div>
</div>
</nav>
<nav class="navbar is-white">
<div class="container">
<div class="navbar-menu">
<div class="navbar-start">
<a class="navbar-item is-active" href="#">Popular</a>
<a class="navbar-item" href="#">Recent</a>
<a class="navbar-item" href="#">Rising</a>
</div>
<div class="navbar-end">
<div class="navbar-item">
<input class="input" type="search" placeholder="Search forum...">
</div>
</div>
</div>
</div>
</nav>
<section class="container">
<div class="columns">
<div class="column is-3">
<a class="button is-primary is-block is-alt is-large" href="#">New Post</a>
<aside class="menu">
<p class="menu-label">
Tags
</p>
<ul class="menu-list">
<li><span class="tag is-primary is-medium ">Dashboard</span></li>
<li><span class="tag is-link is-medium ">Customers</span></li>
<li><span class="tag is-light is-danger is-medium ">Authentication</span></li>
<li><span class="tag is-dark is-medium ">Payments</span></li>
<li><span class="tag is-success is-medium ">Transfers</span></li>
<li><span class="tag is-warning is-medium ">Balance</span></li>
<li><span class="tag is-medium ">Question</span></li>
</ul>
</aside>
</div>
<div class="column is-9">
<div class="box content">
<article class="post">
<h4>Bulma: How do you center a button in a box?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@jsmith</a> replied 34 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 1</span>
</div>
</div>
</article>
<article class="post">
<h4>How can I make a bulma button go full width?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@red</a> replied 40 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 0</span>
</div>
</div>
</article>
<article class="post">
<h4>TypeError: Data must be a string or a buffer when trying touse vue-bulma-tabs</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@jsmith</a> replied 53 minutes ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 13</span>
</div>
</div>
</article>
<article class="post">
<h4>How to vertically center elements in Bulma?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@brown</a> replied 3 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
<article class="post">
<h4>I'm trying to use hamburger menu on bulma css, but it doesn't work. What is wrong?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@hamburgler</a> replied 5 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
<article class="post">
<h4>How to make tiles wrap with Bulma CSS?</h4>
<div class="media">
<div class="media-left">
<p class="image is-32x32">
<img src="http://bulma.io/images/placeholders/128x128.png">
</p>
</div>
<div class="media-content">
<div class="content">
<p>
<a href="#">@rapper</a> replied 3 hours ago &nbsp;
<span class="tag">Question</span>
</p>
</div>
</div>
<div class="media-right">
<span class="has-text-grey-light"><i class="fa fa-comments"></i> 2</span>
</div>
</div>
</article>
</div>
</div>
</div>
</section>
<footer class="footer">
<div class="container">
<div class="content has-text-centered">
<div class="columns is-mobile is-centered">
<div class="field is-grouped is-grouped-multiline">
<div class="control">
<div class="tags has-addons">
<a class="tag is-link" href="https://github.com/BulmaTemplates/bulma-templates">Bulma Templates</a>
<span class="tag is-light">Daniel Supernault</span>
</div>
</div>
<div class="control">
<div class="tags has-addons">
<a class="tag is-link">The source code is licensed</a>
<span class="tag is-light">MIT &nbsp;<i class="fa fa-github"></i></span>
</div>
</div>
</div>
</div>
</div>
</div>
</footer>
<script async type="text/javascript" src="../js/bulma.js"></script>
</body>
</html>
-}
