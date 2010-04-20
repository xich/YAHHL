{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies #-}

module Main where

import Text.YAHHL

import Criterion.Main
import qualified Text.Html as H
import qualified Data.Text.Lazy as T

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

{-
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
-}
