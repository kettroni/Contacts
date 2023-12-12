module Main where

import Relude
import Web.Scotty as S
import Data.Text.Lazy as T
import ContactTemplates
import Contact (mkContact, validateContact)
import Db (getContacts)
import Network.HTTP.Types (created201, status400)

main :: IO ()
main = scotty 3000 $ do
  S.get "/" $ do
    redirect "/contacts"

  S.get "/contacts" $ do
    query :: T.Text <- queryParam "q" `rescueStatusError` (pure . const "")
    S.html $ renderContactPage query getContacts

  S.get "/contacts/new" $ do
    S.html emptyNewContactPage

  S.post "/contacts/new" $ do
    fn    :: T.Text <- formParam "first_name"
    ln    :: T.Text <- formParam "last_name"
    email :: T.Text <- formParam "email"
    pn    :: T.Text <- formParam "phone"

    let c = mkContact fn ln pn email 0

    case validateContact c of
      Left errs -> do
        S.status status400
        S.html $ newContactPage c errs
      _ -> do
        S.status created201
        redirect "/contacts"

rescueStatusError :: ActionM a -> (StatusError -> ActionM a) -> ActionM a
rescueStatusError = rescue
