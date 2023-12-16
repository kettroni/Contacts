module Main where

import Relude hiding (get)
import Web.Scotty
import Data.Text.Lazy as T
import ContactTemplates
import Contact (mkContact, validateContact)
import Db (getContacts)
import Network.HTTP.Types (created201, status400)

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    redirect "/contacts"

  get "/contacts" $ do
    query :: T.Text <- queryParam "q" `rescueStatusError` (pure . const "")
    html $ renderContactPage query getContacts

  get "/contacts/new" $ do
    html emptyNewContactPage

  post "/contacts/new" $ do
    fn    :: T.Text <- formParam "first_name"
    ln    :: T.Text <- formParam "last_name"
    email :: T.Text <- formParam "email"
    pn    :: T.Text <- formParam "phone"

    let c = mkContact fn ln pn email 0

    case validateContact c of
      Left errs -> do
        status status400
        html $ newContactPage c errs
      _ -> do
        status created201
        redirect "/contacts"

rescueStatusError :: ActionM a -> (StatusError -> ActionM a) -> ActionM a
rescueStatusError = rescue
