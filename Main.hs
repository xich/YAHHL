{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module HTreeML where

import Criterion.Main
import qualified Text.Html as H
import qualified Data.Text as T

-- data Tag = forall a . (Render a) => Tag T.Text [Attribute] a
data Tag = Tag T.Text [Attribute] [Tag]
         | Text T.Text
         | EmptyTag

instance Show Tag where
    show = T.unpack . render

data Attribute = Empty
               | Align T.Text

class Render a where
    render :: a -> T.Text
    store  :: a -> [Tag]

instance Render Tag where
    render (Tag n a c) = T.concat [open, n, render a, end, render c, close, n, end]
    render (Text s) = s
    render EmptyTag = T.empty

    store t = [t]

-- goddamn overlapping instances bullshit (see String instance)
-- instance (Render a) => Render [a] where
--     render ts = T.concat $ map render ts

instance Render [Tag] where
    render ts = T.concat $ map render ts

    store ts = ts

instance Render [Attribute] where
    render as = T.concat $ map render as
    store = error "called store on list of attributes"

instance Render Attribute where
    render Empty = T.empty
    render (Align dir) = T.concat [T.pack $ " align=\"", dir, endQuote]
    store = error "called store on an attribute"

instance Render String where
    render = T.pack
    store s = [Text $ T.pack s]

instance (Render a, a ~ Tag) => Render (a -> Tag) where
    render f = error "called render on a function"
    store f = [f EmptyTag]

open = T.pack "<"
close = T.pack "</"
end = T.pack ">"
endQuote = T.pack "\""

-- really would like to be able to take either a Tag, a [Tag], or a String as
-- the third argument. Can't get the type system to let me though.
-- seems like I should be able to do:
-- tag :: (Render a) => String -> [Attribute] -> a -> Tag
-- and modify the data declaration above to:
-- data Tag = forall a . (Render a) => Tag T.Text [Attribute] a
-- tag :: String -> [Attribute] -> [Tag] -> Tag
tag :: (Render a) => String -> [Attribute] -> a -> Tag
tag n a c = Tag (T.pack n) a (store c)

text :: String -> Tag
text s = Text (T.pack s)

align = Align . T.pack

simple = tag "html" [] [tag "body" [] [tag "h1" [] [text "something"], tag "p" [align "right"] [text "blah"]]]
simple2 = html $ body [ h1 "something"
                      , p_a [align "right"] "blah"]

main = defaultMain [ bench "empty" $ nf render $ html body
                   , bench "old-empty" $ nf H.renderHtml $ H.body H.noHtml
                   , bench "simple" $ nf render $ simple
                   , bench "simple2" $ nf render $ simple2
                   ]

html :: (Render a) => a -> Tag
html = tag "html" []
body :: (Render a) => a -> Tag
body = tag "body" []
h1   = tag "h1"   []
p :: (Render a) => a -> Tag
p    = tag "p"    []
p_a  = tag "p"
