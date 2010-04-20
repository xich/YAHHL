module Text.YAHHL.Tags where

import Text.YAHHL.Base

html :: (HTML a) => a -> Tag
html = tag "html"
head_ :: (HTML a) => a -> Tag
head_ = tag "head"
body :: (HTML a) => a -> Tag
body = tag "body"
h1 :: (HTML a) => a -> Tag
h1   = tag "h1"
p :: (HTML a) => a -> Tag
p    = tag "p"
a :: (HTML a) => a -> Tag
a    = tag "a"
title :: (HTML a) => a -> Tag
title = tag "title"
