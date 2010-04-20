{-# LANGUAGE IncoherentInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies #-}

module Text.YAHHL.Base (Entity(..), (#), (#!), HTML(..), Tag(..)) where

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

renderA as = T.concat $ map r as
    where
        r Empty = T.empty
        r (Align dir) = T.concat [T.pack $ " align=\"", dir, endQuote]
        r (Href url)  = T.concat [T.pack $ " href=\"", url, endQuote]

-- So we can write either:
--   1. body "blah"
-- Or:
--   2. body [color "red"] "blah"
-- Or:
--   3. body
class Entity e where
    mkTag :: String -> [Attribute] -> e

-- case 1
instance HTML a => Entity (a -> Tag) where
    mkTag n as t = Tag (T.pack n) as (store t)

-- case 2
instance Entity e => Entity ([Attribute] -> e) where
    mkTag n as a = mkTag n (a++as)

-- case 3
instance Entity Tag where
    mkTag n as = Tag (T.pack n) as []

-- To force the types of the above correctly.
-- TODO: Combine these into one.
infixr 0 #
(#) l r = l $ (id :: Tag -> Tag) r

infixr 0 #!
(#!) l r = l $ (id :: [Tag] -> [Tag]) r
