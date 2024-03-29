module Contact where
import Data.Text.Lazy as T
import Prelude
import Data.Either (lefts)

data Contact = Contact { firstN :: T.Text
                       , lastN :: T.Text
                       , phone :: PhoneNumber
                       , email :: Email
                       , ident :: Int
                       } deriving Show

data Error = FieldIsEmpty T.Text
           | PhoneNumberIsNot10Chars T.Text

type PhoneNumber = T.Text
type Email = T.Text

instance Show Error where
  show (FieldIsEmpty s) = "Field " <> show s <> " is empty."
  show (PhoneNumberIsNot10Chars s) = "Phone number " <> show s <> " is not 10 characters long."

mkNonEmptyTextField :: T.Text -> T.Text -> Either [Error] T.Text
mkNonEmptyTextField field "" = Left [FieldIsEmpty field]
mkNonEmptyTextField field s = Right s

validateFN :: T.Text -> Either [Error] T.Text
validateFN = mkNonEmptyTextField "First Name"

validateLN :: T.Text -> Either [Error] T.Text
validateLN = mkNonEmptyTextField "Last Name"

validatePN :: T.Text -> Either [Error] PhoneNumber
validatePN = mkNonEmptyTextField "Phone number"

validateEmail :: T.Text -> Either [Error] Email
validateEmail = mkNonEmptyTextField "Email"

mkContact :: T.Text -> T.Text -> PhoneNumber -> Email -> Int -> Contact
mkContact = Contact

mkEmptyContact :: Contact
mkEmptyContact = Contact "" "" "" "" 0

validateContact :: Contact -> Either [Error] Contact
validateContact c = case errors of
                       [] -> Right c
                       _ -> Left errors
  where
    errors = mconcat $ lefts [ validateFN (firstN c)
                             , validateLN (lastN c)
                             , validatePN (phone c)
                             , validateEmail (email c)
                             ]
