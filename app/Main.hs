module Main where

import Web.Scotty as S
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html4.FrameSet.Attributes as HA
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy qualified as T
import Relude

main :: IO ()
main = scotty 3000 $ do
  S.get "/" $ do
    redirect "/contacts"

  S.get "/contacts" $ do
    search :: T.Text <- queryParam "q" `resc` (pure . const "")
    html $ renderHtml $ renderContactPage search
      where
        resc = rescue :: ActionM a -> (StatusError -> ActionM a) -> ActionM a

renderContactPage :: T.Text -> H.Html
renderContactPage search = do
  H.docTypeHtml $
    H.html $ do
      H.body $ do
        renderContactSearch search
        renderContacts getContacts

data Contact = Contact { firstN :: T.Text
                        , lastN :: T.Text
                        , phone :: T.Text
                        , email :: T.Text
                        , ident :: Int
                        }

instance H.ToMarkup Contact where
  toMarkup c =
    H.tr $ do
      H.td $ H.toHtml $ firstN c
      H.td $ H.toHtml $ lastN c
      H.td $ H.toHtml $ phone c
      H.td $ H.toHtml $ email c
      H.td $ H.a H.! href ("/contacts/" <> (H.toValue . ident) c <> "/edit") $ "Edit"
      H.td $ H.a H.! href ("/contacts/" <> (H.toValue . ident) c) $ "View"

renderContactSearch :: T.Text -> H.Html
renderContactSearch q = do
  H.form H.! action "/contacts" H.! method "get" H.! class_ "tool-bar" $ do
    H.label H.! for "search" $ "Search Term"
    H.input H.! HA.id "search" H.! HA.type_ "search" H.! HA.name "q" H.! HA.value (H.toValue q)


renderContacts :: [Contact] -> H.Html
renderContacts cs = H.table $ do
      H.thead $ do
        H.tr "First name"
        H.tr "Last name"
        H.tr "Phone number"
        H.tr "Email address"
      H.tbody $ do
        mconcat $ H.toMarkup <$> cs

getContacts :: [Contact]
getContacts = [ Contact { firstN = "Aatti"
                        , lastN = "Aattinen"
                        , phone = "04050605058"
                        , email = "Aatti.Aattinen@Aatti.fi"
                        , ident = 0
                        }
              , Contact { firstN = "Batti"
                        , lastN = "Battinen"
                        , phone = "14050605058"
                        , email = "Batti.Battinen@Batti.fi"
                        , ident = 1
                        }
              , Contact { firstN = "Catti"
                        , lastN = "Cattinen"
                        , phone = "24050605058"
                        , email = "Catti.Cattinen@Catti.fi"
                        , ident = 2
                        }
              , Contact { firstN = "Datti"
                        , lastN = "Dattinen"
                        , phone = "34050605058"
                        , email = "Datti.Dattinen@Datti.fi"
                        , ident = 3
                        }
              ]
