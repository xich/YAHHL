import Hack hiding (body)
import Hack.Contrib.Middleware.ContentLength
import Hack.Contrib.Middleware.ContentType
import Hack.Contrib.Utils
import Hack.Handler.Happstack
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.Lazy as T

import Text.YAHHL

app :: Env -> IO Response
app = \env -> return $                          -- render and change to lazy bytestring
    Response 200 [ ("Content-Type", "text/html") ] (pack $ T.unpack $ render $ page)

-- Need to see what others make sense here.
mw = use [content_length, content_type "text/html"]
main = run (mw app)

page = html [ head_ $ title "happy title"
            , body  [ h1 "happy heading"
                    , a ! [href "url"] $ "a link"
                    , p "don't worry, the link doesn't go anywhere!"
                    ]
            ]
