{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies #-}

module Main where

import Text.YAHHL

import Criterion.Main
import qualified Text.Html as H
import qualified Data.Text.Lazy as T

simple = mkTag "html" [] (mkTag "body" [] [mkTag "h1" [] "something", mkTag "p" [] [align "right"] "blah" :: Tag] :: Tag) :: Tag

simple2 :: Tag
simple2 = html # body [align "center"] # h1 [align "right"] "something"

simple3 :: Tag
simple3 = html # body [align "center"] #! [h1 [align "right"] "something", p "blah"]

ruby = html #! [ head_ # title "happy title"
               , body #! [ h1 "happy heading"
                         , a [href "url"] "a link"
                         ]
               ]

main = defaultMain [ bench "empty"     $ nf render # html # body
                   , bench "old-empty" $ nf H.renderHtml $ H.body H.noHtml
                   , bench "simple"    $ nf render # simple
                   , bench "simple2"   $ nf render # simple2
                   , bench "oldruby"   $ nf H.renderHtml $ (H.header $ H.thetitle $ H.primHtml "happy title")
                                                           H.+++
                                                           (H.body ((H.h1 $ H.primHtml "happy heading")
                                                                   H.+++
                                                                   H.hotlink "url" [H.primHtml "something"]
                                                                   ))
                   , bench "ruby"      $ nf render # ruby
                   ]
