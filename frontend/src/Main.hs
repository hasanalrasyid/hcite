{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
--import           Data.FileEmbed

--import Data.Witherable

import Common

--import Reflex.Dom.Xhr

--import Language.Javascript.JSaddle.Types
--import Control.Monad.IO.Class

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
--  stylesheet "inc/css/bulma-docs.min.css"
  where
    stylesheet l = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", l)
      ]) $ return ()

  {-
main1 = mainWidgetWithCss css body
   where css = $(embedFile "css/tab.css")
-}

-- | Enumeration to track type of page to display
data Page = PageData | PageError
   deriving Eq

data Nav = NavHome
         | NavLanding
         | NavBlog
         | NavAlbum
         | NavKanban
         | NavSearch
         | NavTabs
         deriving (Enum, Show) -- remember, Enum start from 0

navMenu :: [T.Text]
navMenu =   [ "Home"
            , "Landing"
            , "Blog"
            , "Album"
            , "Kanban"
            , "Search"
            , "Tabs"
            ]


--activateDynAttrs :: MonadWidget t m => Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs :: Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs initAttrs b =
  Map.adjust (<> isActive b)  "class" initAttrs
    where isActive True = " is-active"
          isActive _    = ""
            {-
divToggleTopNav :: MonadWidget t m => m (Event t ())
divToggleTopNav = do
  evC <-  toButtonDiv $ do
              el "span" blank
              el "span" blank
              el "span" blank
  return evC

toButtonDiv t = do
  (e, _) <- el' "div" t
  return $ domEvent Click e
-}

toButton :: MonadWidget t m => T.Text -> Dynamic t (Map.Map T.Text T.Text) -> m a -> m (Event t ())
toButton d a t = do
  (e,_) <- elDynAttr' d a t
  return $ domEvent Click e

bodyNav :: MonadWidget t m => m (Dynamic t Nav)
bodyNav =
  elClass "nav" "navbar topNav" $ do
    elClass "div" "container" $ mdo
      dynToggleTopNav <- toggle False evToggleTopNav
      evToggleTopNav <- elClass "div" "navbar-brand" $ do
        elClass "a" "navbar-item" $ -- href="../">
          el "h1" $ text "HCITE"
          -- elAttr "img" (("src" =: "inc/img/bulma.png") <> ("width" =: "112") <> ("height"=:"28")) blank
        toButton "div" (activateDynAttrs (("class" =: "navbar-burger burger") <> ("data-target" =: "topNav")) <$> dynToggleTopNav) $ do
              el "span" blank
              el "span" blank
              el "span" blank
--        </div>
--      </div>
--    </div>
      elDynAttr "div" (activateDynAttrs (( "class" =: "navbar-menu") <> ("id" =: "topNav")) <$> dynToggleTopNav) $ do
        evNav1 <- elClass "div" "navbar-start" $ do
          evNavR0 <- mapM (\t -> toButton "a" (constDyn ("class" =: "navbar-item")) $ text t) navMenu
          holdDyn NavHome $ fmap toEnum $ leftmost $ zipWith (<$) [0..] evNavR0

        elClass "div" "navbar-end" $ do
          elClass "div" "navbar-item" $ do
            elClass "div" "field is-grouped"$ do
              elClass "p" "control" $ do
                elClass "a" "button is-small" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user-plus" blank
                  el "span" $ text "Register"
              elClass "p" "control" $ do
                elClass "a" "button is-small is-info is-outlined" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user" blank
                  el "span" $ text "Login"
        return evNav1

  {- not needed
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
  -}

bodySection :: MonadWidget t m => m ()
bodySection = do
  elClass "section" "container" $ do
    elClass "div" "columns" $ do
      elClass "div" "column is-3" $ blank -- do
        {-
        elAttr "a" (("class" =: "button is-primary is-block is-alt is-large") <> ("href" =: "#")) $ text "New Post"
        elClass "aside" "menu" $ do
          elClass "p" "menu-label" $ text "Tags"
          elClass "ul" "menu-list" $ do
            el "li" $ elClass "span" "tag is-primary is-medium "         $ text "Dashboard"
            el "li" $ elClass "span" "tag is-link is-medium "            $ text "Customers"
            el "li" $ elClass "span" "tag is-light is-danger is-medium " $ text "Authentication"
            el "li" $ elClass "span" "tag is-dark is-medium "            $ text "Payments"
            el "li" $ elClass "span" "tag is-success is-medium "         $ text "Transfers"
            el "li" $ elClass "span" "tag is-warning is-medium "         $ text "Balance"
            el "li" $ elClass "span" "tag is-medium "                    $ text "Question"
            -}
--    </div>
      elClass "div" "column is-9" $ do
        elClass "div" "box content" $ do
          bodySectionArticles
          bodySectionArticles
          bodySectionArticles
          bodySectionArticles
          bodySectionArticles
          bodySectionArticles
          bodySectionArticles

bodySectionArticles :: MonadWidget t m => m ()
bodySectionArticles = do
          elClass "article" "post" $ do
            el "h4" $ text "Bulma: How do you center a button in a box?"
            elClass "div" "media" $ do
              elClass "div" "media-left" $ do
                elClass "p" "image is-32x32" $ do
                  elAttr "img" ("src" =: "http://bulma.io/images/placeholders/128x128.png") blank
--            </div>
              elClass "div" "media-content" $ do
                elClass "div" "content" $ do
                  el "p" $ do
                    elAttr "a" ("href" =: "#") $ text "@jsmith"
                    text " replied 34 minutes ago &nbsp;"
                    elClass "span" "tag" $ text "Question"
--                </p>
--              </div>
--            </div>
              elClass "div" "media-right" $ do
                elClass "span" "has-text-grey-light" $ do
                  elClass "i" "fa fa-comments" blank
                  text "1"
--            </div>
--          </div>
--        </article>
          elClass "article" "post" $ do
            el "h4" $ text "This is second article?"
            elClass "div" "media" $ do
              elClass "div" "media-left" $ do
                elClass "p" "image is-32x32" $ do
                  elAttr "img" ("src" =: "http://bulma.io/images/placeholders/128x128.png") blank
--            </div>
              elClass "div" "media-content" $ do
                elClass "div" "content" $ do
                  el "p" $ do
                    elAttr "a" ("href" =: "#") $ text "@jsmith"
                    text " replied 34 minutes ago &nbsp;"
                    elClass "span" "tag" $ text "Question"
--                </p>
--              </div>
--            </div>
              elClass "div" "media-right" $ do
                elClass "span" "has-text-grey-light" $ do
                  elClass "i" "fa fa-comments" blank
                  text "1"
--            </div>
--          </div>
--        </article>

bodyFooter :: MonadWidget t m => m ()
bodyFooter = do
  elClass "footer" "footer" $ do
    elClass "div" "container" $ do
      elClass "div" "content has-text-centered" $ do
        elClass "div" "columns is-mobile is-centered" $ do
          elClass "div" "field is-grouped is-grouped-multiline" $ do
            elClass "div" "control" $ do
              elClass "div" "tags has-addons" $ do
                elAttr "a" (("class" =: "tag is-link") <> ("href" =: "https://github.com/BulmaTemplates/bulma-templates")) $ text "Bulma Templates"
                elClass "span" "tag " $ text "Daniel Supernault"
--            </div>
--          </div>
            elClass "div" "control" $ do
              elClass "div" "tags has-addons" $ do
                elAttr "a" (("class" =: "tag is-link")) $ text "The source code is licensed"
                elClass "span" "tag " $ text "MIT &nbsp;"
                elClass "i" "fa fa-github" blank
                  {-
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </footer>
    -}
    {- unused
  <script async type="text/javascript" src="../js/bulma.js"></script>
  </body>
-}

bodyModal :: MonadWidget t m => Dynamic t Bool -> Modal -> m () -> m ()
bodyModal dynToggleModal ty b = do
  elDynAttr "div" (activateDynAttrs ("class" =: "modal") <$> dynToggleModal) $ do
    elClass "div" "modal-background" blank
    elClass "div" (mode ty) b
  where
    mode ModalSimple = "modal-content"
    mode ModalCard   = "modal-card"

pageLogin :: MonadWidget t m => m ()
pageLogin = do
  elClass "div" "hero is-success is-fullheight" $ do
    elClass "div" "hero-body" $ do
      elClass "div" "container has-text-centered" $ do
        elClass "div" "column is-4 is-offset-4" $ do
          elClass "h3" "title has-text-black" $ text "Login"
          elClass "hr" "login-hr" blank
          elClass "p" "subtitle has-text-black" $ text "Please login to proceed"
          elClass "div" "box" $ do
            elClass "figure" "avatar" $ do
              elAttr "img" ("src" =: "https://placehold.it/128x128") blank
            el "form" $ do
              elClass "div" "field" $ do
                elClass "div" "control" $ do
                  elAttr "input" (Map.fromList [("class","input is-large"),("type","email"),("placeholder","Your Email"),("autofocus","")]) blank
              elClass "div" "field" $ do
                elClass "div" "control" $ do
                  elAttr "input" (Map.fromList [("class","input is-large"),("type","password"),("placeholder","Your Password")]) blank
              elClass "div" "field" $ do
                elClass "div" "checkbox" $ do
                  elAttr "input" ("type" =: "password") $ text "Remember me"
              elClass "button" "button is-block is-info is-large is-fullwidth" $ do
                text "Login"
                elAttr "i" (Map.fromList [("class", "fa fa-sign-in"),("aria-hidden","true")]) $ text "::before"
          elClass "p" "has-text-grey" $ do
            el "a" $ text "Sign up"
            el "a" $ text "Forgot Password"
            el "a" $ text "Need Help?"


pageDetail :: MonadWidget t m => m ()
pageDetail = do
    elClass "header" "modal-card-head" $ do
      elClass "p" "modal-card-title" $ text "Modal title"
      elAttr "button" (("class" =: "delete") <> ("aria-label" =: "close")) blank
    elClass "section" "modal-card-body" $ do
      elClass "div" "content" $ do
        el "h1" $ text "Hello World"
        el "p" $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla accumsan, metus ultrices eleifend gravida, nulla nunc varius lectus, nec rutrum justo nibh eu lectus. Ut vulputate semper dui. Fusce erat odio, sollicitudin vel erat vel, interdum mattis neque."
        el "h2" $ text "Second level_/h2 $ do"
        el "p" $ text "Curabitur accumsan turpis pharetra elClass <strong>augue tincidunt_/strong> blandit. Quisque condimentum maximus mi, sit amet commodo arcu rutrum id. Proin pretium urna vel cursus venenatis. Suspendisse potenti. Etiam mattis sem rhoncus lacus dapibus facilisis. Donec at dignissim dui. Ut et neque nisl._/p $ do"
        el "ul" $ do
          el "li" $ text "In fermentum leo eu lectus mollis, quis dictum mi aliquet._/li $ do"
          el "li" $ text "Morbi eu nulla lobortis, lobortis est in, fringilla felis._/li $ do"
          el "li" $ text "Aliquam nec felis in sapien venenatis viverra fermentum nec lectus._/li $ do"
          el "li" $ text "Ut non enim metus._/li $ do"
        el "h3" $ text " Third level_/h3 $ do"
        el "p" $ text "Quisque ante lacus, malesuada ac auctor vitae, congue elClass "
        el "ol" $ do
          el "li" $ text ">Donec blandit a lorem id convallis._/li $ do"
          el "li" $ text ">Cras gravida arcu at diam gravida gravida._/li $ do"
          el "li" $ text ">Integer in volutpat libero._/li $ do"
          el "li" $ text ">Donec a diam tellus._/li $ do"
          el "li" $ text ">Aenean nec tortor orci._/li $ do"
          el "li" $ text ">Quisque aliquam cursus urna, non bibendum massa viverra eget._/li $ do"
          el "li" $ text ">Vivamus maximus ultricies pulvinar._/li $ do"
        el "blockquote" $ text " Ut venenatis, nisl scelerisque sollicitudin fermentum, quam libero hendrerit ipsum, ut blandit est tellus sit amet turpis._/blockquote $ do"
        el "p" $ text "Quisque at semper enim, eu hendrerit odio. Etiam auctor nisl et elClass"
        el "p" $ text " Sed sagittis enim ac tortor maximus rutrum. Nulla facilisi. Donec mattis vulputate risus in luctus. Maecenas vestibulum interdum commodo._/p $ do"
        el "p" $ text " Suspendisse egestas sapien non felis placerat elementum. Morbi tortor nisl, suscipit sed mi sit amet, mollis malesuada nulla. Nulla facilisi. Nullam ac erat ante._/p $ do"
        el "h4"$ text ">Fourth level_/h4 $ do"
        el "p" $ text ">Nulla efficitur eleifend nisi, sit amet bibendum sapien fringilla ac. Mauris euismod metus a tellus laoreet, at elementum ex efficitur._/p $ do"
        el "p" $ text ">Maecenas eleifend sollicitudin dui, faucibus sollicitudin augue cursus non. Ut finibus eleifend arcu ut vehicula. Mauris eu est maximus est porta condimentum in eu justo. Nulla id iaculis sapien._/p $ do"
        el "p" $ text ">Phasellus porttitor enim id metus volutpat ultricies. Ut nisi nunc, blandit sed dapibus at, vestibulum in felis. Etiam iaculis lorem ac nibh bibendum rhoncus. Nam interdum efficitur ligula sit amet ullamcorper. Etiam tristique, leo vitae porta faucibus, mi lacus laoreet metus, at cursus leo est vel tellus. Sed ac posuere est. Nunc ultricies nunc neque, vitae ultricies ex sodales quis. Aliquam eu nibh in libero accumsan pulvinar. Nullam nec nisl placerat, pretium metus vel, euismod ipsum. Proin tempor cursus nisl vel condimentum. Nam pharetra varius metus non pellentesque._/p $ do"
        el "h5"$ text ">Fifth level_/h5 $ do"
        el "p" $ text ">Aliquam sagittis rhoncus vulputate. Cras non luctus sem, sed tincidunt ligula. Vestibulum at nunc elit. Praesent aliquet ligula mi, in luctus elit volutpat porta. Phasellus molestie diam vel nisi sodales, a eleifend augue laoreet. Sed nec eleifend justo. Nam et sollicitudin odio._/p $ do"
        el "h6"$ text ">Sixth level_/h6 $ do"
        el "p" $ text ">Cras in nibh lacinia, venenatis nisi et, auctor urna. Donec pulvinar lacus sed diam dignissim, ut eleifend eros accumsan. Phasellus non tortor eros. Ut sed rutrum lacus. Etiam purus nunc, scelerisque quis enim vitae, malesuada ultrices turpis. Nunc vitae maximus purus, nec consectetur dui. Suspendisse euismod, elit vel rutrum commodo, ipsum tortor maximus dui, sed varius sapien odio vitae est. Etiam at cursus metus._/p $ do"
    elClass "footer" "modal-card-foot" $ do
      elClass "button" "button is-success" $ text "Save changes"
      elClass "button" "button" $ text "Cancel"

pageNotification :: MonadWidget t m => m ()
pageNotification = do
    elClass "div" "box" $ do
      elClass "article" "media" $ do
        elClass "div" "media-left" $ do
          elClass "figure" "image is-64x64" $ do
            elAttr "img" (("src" =: "https://bulma.io/images/placeholders/128x128.png") <> ("alt" =: "Image")) blank
        elClass "div" "media-content" $ do
          elClass "div" "content" $ do
            el "p" $ do
              text "John Smith"
              el "br" blank
              text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean efficitur sit amet massa fringilla egestas. Nullam condimentum luctus turpis."
          elClass "nav" "level is-mobile" $ do
            elClass "div" "level-left" $ do
              elAttr "a" (("class" =: "level-item") <> ("aria-label" =: "retweet")) $ do
                  elClass "span" "icon is-small" blank {- $ do
                    elAttr "svg" (Map.fromList [("class", "svg-inline--fa fa-retweet fa-w-20")
                                               ,("aria-hidden" =: "true"),("data-prefix","fas"),("data-icon", "retweet"),("role","img")
                                               ,("xmlns","http://www.w3.org/2000/svg"),( "viewBox","0 0 640 512"),("data-fa-i2svg","")
                                               ]) $ do
                      elAttr "path" (Map.fromList [("fill","currentColor")
                                                  ,("d","M629.657 343.598L528.971 444.284c-9.373 9.372-24.568 9.372-33.941 0L394.343 343.598c-9.373-9.373-9.373-24.569 0-33.941l10.823-10.823c9.562-9.562 25.133-9.34 34.419.492L480 342.118V160H292.451a24.005 24.005 0 0 1-16.971-7.029l-16-16C244.361 121.851 255.069 96 276.451 96H520c13.255 0 24 10.745 24 24v222.118l40.416-42.792c9.285-9.831 24.856-10.054 34.419-.492l10.823 10.823c9.372 9.372 9.372 24.569-.001 33.941zm-265.138 15.431A23.999 23.999 0 0 0 347.548 352H160V169.881l40.416 42.792c9.286 9.831 24.856 10.054 34.419.491l10.822-10.822c9.373-9.373 9.373-24.569 0-33.941L144.971 67.716c-9.373-9.373-24.569-9.373-33.941 0L10.343 168.402c-9.373 9.373-9.373 24.569 0 33.941l10.822 10.822c9.562 9.562 25.133 9.34 34.419-.491L96 169.881V392c0 13.255 10.745 24 24 24h243.549c21.382 0 32.09-25.851 16.971-40.971l-16.001-16z")
                                                  ,("style","--darkreader-inline-fill:currentColor;")
                                                  ,("data-darkreader-inline-fill","")
                                                  ]) blank
                elAttr "a" (("class" =: "level-item") <> ("aria-label" =: "like")) $ do
                  elClass "span" "icon is-small" $ do
                    elAttr "svg" (Map.fromList [("class", "svg-inline--fa fa-heart fa-w-16")
                                               ,("aria-hidden" =: "true"),("data-prefix","fas"),("data-icon", "heart"),("role","img")
                                               ,("xmlns","http://www.w3.org/2000/svg"),( "viewBox","0 0 512 512"),("data-fa-i2svg","")
                                               ]) $ do
                      elAttr "path" (Map.fromList [("fill","currentColor")
                                                  ,("d","M462.3 62.6C407.5 15.9 326 24.3 275.7 76.2L256 96.5l-19.7-20.3C186.1 24.3 104.5 15.9 49.7 62.6c-62.8 53.6-66.1 149.8-9.9 207.9l193.5 199.8c12.5 12.9 32.8 12.9 45.3 0l193.5-199.8c56.3-58.1 53-154.3-9.8-207.9z")
                                                  ,("style","--darkreader-inline-fill:currentColor;")
                                                  ,("data-darkreader-inline-fill","")
                                                  ]) blank
-}


data Modal = ModalSimple | ModalCard deriving Show

body :: MonadWidget t m => m ()
body  = mdo
  el "h2" $ text "Swiss Weather Data (Tab display)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  el "p" blank
  -- Build and send the request
  evStart <- getPostBuild

  dynSelectedNav <- bodyNav

  bodySection




--  bodyModal ModalSimple pageLogin
--  bodyModal ModalCard   pageDetail
  display dynSelectedNav
  let evModal = never
  dynToggleModal <- toggle False evModal
  bodyModal dynToggleModal ModalSimple pageNotification
  bodyFooter

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

    {-
-- | Display the meteo data in a tabbed display
pageData :: (PostBuild t m, DomBuilder t m) => Event t XhrResponse -> Dynamic t Page -> m ()
--pageData evOk dynPage = do
pageData _ dynPage = do
--  evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evOk
--  let evSmnStat = fmapMaybe smnStation evSmnRec
  let dynAttr = visible <$> dynPage <*> pure PageData
  elDynAttr "div" dynAttr $ text "PageData"
--    tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat
-}

-- | Display the error page
pageErr :: MonadWidget t m => Event t (Maybe SmnRecord) -> Dynamic t Page -> m ()
pageErr evErr dynPage = do
  let dynAttr = visible <$> dynPage <*> pure PageError
  elDynAttr "div" dynAttr $ do
     el "h3" $ text "Error"
     dynText =<< holdDyn "" ( const "PageError" <$> evErr)

  {-
-- | Split up good and bad response events
checkXhrRsp :: (Filterable f, FunctorMaybe f) => f XhrResponse -> (f XhrResponse, f XhrResponse)
checkXhrRsp evRsp = (evOk, evErr)
  where
    evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
    evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp
-}

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

  {-
-- | Create a tabbed display
--tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evMeteo evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabMeteo evMeteo))]

-}
-- | Create the DOM elements for the Station tab
--tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat :: (DomBuilder t m, PostBuild t m, MonadHold t m) =>
           Event t SmnStation -> m ()
tabStat evStat = do
  dispStatField "Code" staCode evStat
  dispStatField "Name" staName evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Meteo data tab
--tabMeteo :: MonadWidget t m => Event t SmnRecord -> m ()
tabMeteo :: (DomBuilder t m, PostBuild t m, MonadHold t m) =>
            Event t SmnRecord -> m ()
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
dispStatField :: (DomBuilder t m, PostBuild t m, MonadHold t m) =>
                 T.Text -> (a -> T.Text) -> Event t a -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- | Display a single field from the SmnRecord record
--dispMeteoField :: MonadWidget t m => T.Text -> (SmnRecord -> T.Text) -> Event t SmnRecord -> m ()
dispMeteoField :: (DomBuilder t m, PostBuild t m, MonadHold t m) =>
                  T.Text -> (a -> T.Text) -> Event t a -> m ()
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

