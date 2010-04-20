{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies #-}

module Main where

import Criterion.Main
import qualified Text.Html as H
import qualified Data.Text.Lazy as T

-- Tried to do the 'appending trick' (think ShowS) but found it to actually
-- be slower... which makes sense. Data.Text.append makes a new
-- array and copies each string into it, so we don't really gain anything
-- by being clever.

-- data Tag = forall a . (HTML a) => Tag T.Text [Attribute] a
data Tag = Tag T.Text [Attribute] [Tag]
         | Text T.Text
         | EmptyTag

instance Show Tag where
    show = T.unpack . render

data Attribute = Empty
               | Align T.Text
               | Href T.Text

class HTML a where
    render :: a -> T.Text
    store  :: a -> [Tag]

-- For multiple children
instance (HTML a) => HTML [a] where
    render ts = T.concat $ map render ts
    store  ts = concatMap store ts

-- Single tag
instance HTML Tag where
    render (Tag n a c) = T.concat [open, n, renderA a, end, render c, close, n, end]
    render (Text s) = s
    render EmptyTag = T.empty

    store t = [t]

{-- Single attribute
instance HTML Attribute where
    render Empty = T.empty
    render (Align dir) = T.concat [T.pack $ " align=\"", dir, endQuote]
    render (Href url)  = T.concat [T.pack $ " href=\"", url, endQuote]
    store = error "called store on an attribute"
-}
renderA as = T.concat $ map r as
    where
        r Empty = T.empty
        r (Align dir) = T.concat [T.pack $ " align=\"", dir, endQuote]
        r (Href url)  = T.concat [T.pack $ " href=\"", url, endQuote]

-- For Text
instance HTML [Char] where
    render = T.pack
    store s = [Text $ T.pack s]

instance HTML T.Text where
    render = id
    store t = [Text t]

-- So we can do empty tag groups, like: html body => <html><body></body></html>
instance (HTML a, a ~ Tag) => HTML (a -> Tag) where
    render f = error "called render on a function"
    store f = [f EmptyTag]

open = T.pack "<"
close = T.pack "</"
end = T.pack ">"
endQuote = T.pack "\""

-- Don't really like this solution but living with it for now.
-- tag :: (HTML a) => String -> [Attribute] -> a -> Tag
-- tag n a c = Tag (T.pack n) a (store c)
tag :: (HTML a) => String -> a -> Tag
tag n c = Tag (T.pack n) [] (store c)

class AddAttr a where
    (!) :: a -> [Attribute] -> a

instance (AddAttr b) => AddAttr (a -> b) where
    fn ! attrs = \ cs -> fn cs ! attrs

instance AddAttr Tag where
    (Tag n a c) ! attr = Tag n (a ++ attr) c
    other       ! _    = other

infixl 8 !
-- End solution I don't like.

{-
class TagContent a b | a -> b where
    mkTag :: (HTML b) => String -> a -> b

instance (HTML a) => TagContent [Attribute] (a -> Tag) where
    mkTag name as = tag name as

instance (HTML c) => TagContent c Tag where
    mkTag name cs = tag name [] cs

simple3 = mkTag "html" (tag "body" [] [tag "h1" [] "something", tag "p" [align "right"] "blah"])
-}
align = Align . T.pack
href  = Href  . T.pack

simple = tag "html" $ tag "body" [tag "h1" "something", tag "p" ! [align "right"] $ "blah" ]
simple2 = html $ body [ h1 "something"
                      , p ! [align "right"] $ "blah"]

ruby = html [ head_ $ title "happy title"
            , body [ h1 "happy heading"
                   , a ! [href "url"] $ "a link"
                   ]
            ]

main = defaultMain [ bench "empty"     $ nf render $ html $ body
                   , bench "old-empty" $ nf H.renderHtml $ H.body H.noHtml
                   , bench "simple"    $ nf render $ simple
                   , bench "simple2"   $ nf render $ simple2
                   , bench "oldruby"   $ nf H.renderHtml $ (H.header $ H.thetitle $ H.primHtml "happy title")
                                                           H.+++
                                                           (H.body ((H.h1 $ H.primHtml "happy heading")
                                                                   H.+++
                                                                   H.hotlink "url" [H.primHtml "something"]
                                                                   ))
                   , bench "ruby"      $ nf render $ ruby
                   ]

html :: (HTML a) => a -> Tag
html = tag "html"
head_ = tag "head"
body :: (HTML a) => a -> Tag
body = tag "body"
h1   = tag "h1"
p :: (HTML a) => a -> Tag
p    = tag "p"
a    = tag "a"
title = tag "title"
