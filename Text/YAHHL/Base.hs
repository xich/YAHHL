{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies #-}

module Text.YAHHL.Base (tag, (!), HTML(..), Tag(..)) where

import Text.YAHHL.Attribute
import Text.YAHHL.CAF

import qualified Data.Text.Lazy as T

-- Tried to do the 'appending trick' (think ShowS) but found it to actually
-- be slower... which makes sense. Data.Text.append makes a new
-- array and copies each string into it, so we don't really gain anything
-- by being clever.

data Tag = Tag T.Text [Attribute] [Tag]
         | Text T.Text
         | EmptyTag

instance Show Tag where
    show = T.unpack . render

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

-- For Text
instance HTML [Char] where
    render = T.pack
    store s = [Text $ T.pack s]

instance HTML T.Text where
    render = id
    store t = [Text t]

-- So we can do empty tag groups, like: html body => <html><body></body></html>
instance (HTML a, a ~ Tag) => HTML (a -> Tag) where
    render f = render $ f EmptyTag
    store f = [f EmptyTag]

tag :: (HTML a) => String -> a -> Tag
tag n c = Tag (T.pack n) [] (store c)

{- Was trying to get this syntax to work: p "foo" and p [align "right"] "foo"
tag :: (HTML a) => String -> [Attribute] -> a -> Tag
tag n a c = Tag (T.pack n) a (store c)

class TagContent a b | a -> b where
    mkTag :: (HTML b) => String -> a -> b

instance (HTML a) => TagContent [Attribute] (a -> Tag) where
    mkTag name as = tag name as

instance (HTML c) => TagContent c Tag where
    mkTag name cs = tag name [] cs

simple3 = mkTag "html" (tag "body" [] [tag "h1" [] "something", tag "p" [align "right"] "blah"])
-}

renderA as = T.concat $ map r as
    where
        r Empty = T.empty
        r (Align dir) = T.concat [T.pack $ " align=\"", dir, endQuote]
        r (Href url)  = T.concat [T.pack $ " href=\"", url, endQuote]

-- Don't really like this solution but living with it for now.
class AddAttr a where
    (!) :: a -> [Attribute] -> a

instance (AddAttr b) => AddAttr (a -> b) where
    fn ! attrs = \ cs -> fn cs ! attrs

instance AddAttr Tag where
    (Tag n a c) ! attr = Tag n (a ++ attr) c
    other       ! _    = other

infixl 8 !
-- End solution I don't like.
