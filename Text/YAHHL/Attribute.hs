module Text.YAHHL.Attribute where

import qualified Data.Text.Lazy as T

data Attribute = Empty
               | Align T.Text
               | Href T.Text

-- List of attributes.
align = Align . T.pack
href  = Href  . T.pack

