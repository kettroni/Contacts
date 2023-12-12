module Db where
import Contact
import Relude
import Data.Text.Lazy (singleton)

getContacts :: [Contact]
getContacts = do
  i <- [65..85]
  let x = singleton $ chr i
  pure $ mkContact x x x x i
