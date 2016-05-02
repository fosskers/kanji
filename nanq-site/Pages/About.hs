{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.About where

import Lucid
import Pages.Bootstrap
import Text.Printf.TH

---

about :: Html ()
about = row_ $ col6_ [class_ "col-md-offset-3"] $ do
  theSite
  hr_ []
  kanken
  hr_ []
  me

theSite :: Html ()
theSite = do
  h4_ "About the Site"
  p_ . toHtml $ [lt|This is a website for learners and native speakers of
                   Japanese to analyse bodies of Japanese text for
                   difficultly. Here we measure difficulty by a number of
                   factors:|]
  ul_ $ do
    li_ "Density of Kanji vs non-Kanji in the text"
    li_ "Density of Kanji learned in Japanese elementary school within the text"
    li_ $ do
      "Appearance of Kanji from higher levels of the "
      a_ [href_ "http://www.kanken.or.jp/kanken/"]
        "Japan Kanji Aptitude Test (Kanken)"
  p_ . toHtml $ [lt|Want to read a book or manga, and would like to know
                   up-front what Kanji you'll need to learn?|]
  p_ "Want to be better prepared for your next Kanken?"
  p_ "Interested in finding out how hard a “hard“ text really is?"
  p_ "Then please enjoy this site."
  p_ $ i_ $ do
    "This website is written completely in "
    a_ [href_ "https://www.haskell.org"] "Haskell"
    " and is powered by the "
    a_ [href_ "http://haskell-servant.readthedocs.io/en/stable/"] "servant"
    " and "
    a_ [href_ "http://hackage.haskell.org/package/kanji"] "kanji"
    " libraries."

kanken :: Html ()
kanken = do
  h4_ "About the Japan Kanji Aptitude Test"
  p_ . toHtml $
    [lt|The Japan Kanji Aptitude Test (漢字検定) is an exam held multiple
       times per year all over Japan. It has levels from 10 to 1
       with 1 being the highest, also including a “2.5” and a “1.5”
       level, making a total of twelve levels. The levels are split
       roughly according to groups of Kanji learned at each level of
       public schooling, so levels and grades can be associated
       (e.g. Level 8 is 3rd Grade proficiency).|]
  p_ . toHtml $
    [lt|The organization which runs the exam, the Japan Kanji
       Aptitude Testing Public Interest Foundation, claims that
       Level 2 is “standard adult level”. Level 2 is fairly
       difficult even for native speakers, and cannot be obtained
       without considerable study, despite being considered
       “standard”.|]
  p_. toHtml $
    [lt|Many foreign learners of Japanese also take this exam to
       gauge their own progress. For learners and native speakers
       alike, this website provides them with difficulty information
       of a text they submit, as defined by the Levels from the
       Aptitude Test. This will help learners determine if a certain
       text is above or below their level, and should also prove if
       “Level 2” is the real standard level.|]

me :: Html ()
me = do
  h4_ "About Me"
  p_ $ do
    "My Japanese journey:"
    ul_ $ do
      li_ "2006: Started learning Japanese outside of school"
      li_ "Late 2000s: Translated Bleach manga for Maximum7"
      li_ "2008-2009: University exchange to Saga, Japan"
      li_ "2010-2013: Taught English in Sasebo, Japan"
      li_ "2011: Obtained JLPT N1"
      li_ "2013: Obtained Level Pre-2 of the Japan Kanji Aptitude Test"
    toHtml [lt|I'm an avid speaker of Kyushu dialects and a fan of Western
              Japanese ones. I currently work as a software developer
              in Vancouver, Canada.|]
