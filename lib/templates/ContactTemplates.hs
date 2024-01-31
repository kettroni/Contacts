module ContactTemplates where

import Relude hiding (head, span)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5.Attributes qualified as HA
import Data.Text.Lazy qualified as T
import Contact

emptyNewContactPage :: T.Text
emptyNewContactPage = newContactPage mkEmptyContact []

newContactPage :: Contact -> [Error] -> T.Text
newContactPage contact errs = renderHtml $ do
  -- docTypeHtml $ do
  --   head styleHeaders

    body $ do
      form ! HA.action "/contacts/new" ! HA.method "post" $ do
        fieldset $ do
          legend "Contact Values"
          p $ do
            label ! HA.for "email" $ "Email"
            input ! HA.name "email" ! HA.id "email" ! HA.type_ "email" ! HA.placeholder "Email" ! HA.value (toValue $ email contact)

          p $ do
            label ! HA.for "first_name" $ "First Name"
            input ! HA.name "first_name" ! HA.id "first_name" ! HA.type_ "first_name" ! HA.placeholder "First Name" ! HA.value (toValue $ firstN contact)

          p $ do
            label ! HA.for "last_name" $ "Last Name"
            input ! HA.name "last_name" ! HA.id "last_name" ! HA.type_ "last_name" ! HA.placeholder "Last Name" ! HA.value (toValue $ lastN contact)

          p $ do
            label ! HA.for "phone" $ "Phone"
            input ! HA.name "phone" ! HA.id "phone" ! HA.type_ "phone" ! HA.placeholder "Phone" ! HA.value (toValue $ phone contact)

          errorSpan errs

          button ! btnPri $ "Save"
      p $ a ! btnSnd ! HA.href "/contacts" $ "Back"

errorSpan :: [Error] -> Html
errorSpan errs = toHtml $ (\e -> span ! HA.class_ "error" $ show e) <$> errs

renderContactPage :: T.Text -> [Contact] -> T.Text
renderContactPage query contacts = renderHtml $ do
  docTypeHtml $ do
    head styleHeaders

    body $ do
      contactSearch query
      contactsTable contacts
      a ! btnPri ! HA.href "/contacts/new" $ "Add Contact"

contactSearch :: T.Text -> Html
contactSearch query = do
  form ! HA.action "/contacts" ! HA.method "get" ! HA.class_ "tool-bar" $ do
    label ! HA.for "search" $ "Search Term"
    input ! HA.id "search" ! HA.type_ "search" ! HA.name "q" ! HA.value (toValue query)

contactsTable :: [Contact] -> Html
contactsTable cs = table $ do
  thead $ do
    th "First name"
    th "Last name"
    th "Phone number"
    th "Email address"
  tbody $ do
    mconcat $ contactTableRow <$> cs

contactTableRow :: Contact -> Html
contactTableRow c = tr ! HA.class_ "odd:bg-white even:bg-slate-50" $ do
  td $ toHtml $ firstN c
  td $ toHtml $ lastN c
  td $ toHtml $ phone c
  td $ toHtml $ email c
  td $ a ! HA.href ("/contacts/" <> (toValue . ident) c <> "/edit") $ "Edit"
  td $ a ! HA.href ("/contacts/" <> (toValue . ident) c) $ "View"

styleHeaders :: Html
styleHeaders = mempty
--tailwindHeaders

tailwindHeaders :: Html
tailwindHeaders = do
  script ! HA.src "https://cdn.tailwindcss.com" $ ""

btnPri :: Attribute
btnPri = HA.class_ "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded"

btnSnd :: Attribute
btnSnd = HA.class_ "bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
