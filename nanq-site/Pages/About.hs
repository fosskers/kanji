{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Pages.About where

import Data.Foldable
import Lucid
import Pages.Bootstrap
import Text.Printf.TH

---

about :: Html ()
about = row_ $ left *> right
  where left = col5_ [class_ "col-md-offset-1"] $ fold [ theSite, hr_ [], kanken, hr_ [], me ]
        right = col5_ $ fold [theSiteJ, hr_ [], kankenJ, hr_ [], meJ ]

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
  p_ . toHtml $ [lt|Then please enjoy this site. Please be aware that it
                   is by no means “done“, and that I have plans for further
                   development.|]
  p_ $ i_ $ do
    "This website is written completely in "
    a_ [href_ "https://www.haskell.org"] "Haskell"
    " and is powered by the "
    a_ [href_ "http://haskell-servant.readthedocs.io/en/stable/"] "servant"
    " and "
    a_ [href_ "http://hackage.haskell.org/package/kanji"] "kanji"
    " libraries."

theSiteJ :: Html ()
theSiteJ = do
  h4_ "本サイトについて"
  p_ . toHtml $ [lt|このサイトは日本語のネイティブも学生も日本語の
                   文章の漢字難易度の分析ができるためにある。「難易度」
                   とは下記の点から定義される：
                   |]
  ul_ $ do
    li_ "文章中の漢字の密度"
    li_ "小学校で学ぶ漢字の密度"
    li_ $ do
      a_ [href_ "http://www.kanken.or.jp/kanken/"] "漢字検定"
      "の上級の漢字の有無"
  p_ "このサイトの便利な時："
  ul_ $ do
    li_ "本や漫画を読んで、どの知らない漢字出るか知りたい時"
    li_ "迫る漢検の準備をばっちりにしたい時"
    li_ "ある文章の本当の難易度が知りたい時"
  p_ $ i_ "このサイトは全て「Haskell」で作られた。"

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

kankenJ :: Html ()
kankenJ = do
  h4_ "漢字検定について"
  p_ . toHtml $
    [lt|漢字検定は日本で年に数回開催される試験である。下の10級から上の1級まで
       あり、中の準二級と準一級を含んで計12級がある。学級と関連があるよう、
       下の級は学級別に別れている（例：8級は小学校３年生程度）。|]
  p_ . toHtml $
    [lt|日本漢字検定協会が主張するには、漢検の二級が「高校卒業・大学・または一般程度」
       である。「一般程度」といえど、相当な勉強をしないとネイティブでも
       合格しにくい。|]
  p_ . toHtml $
    [lt|ネイティブのためにも異国の学生のためにも、二級が本当の一般程度で
       あるか、これを解決するのはこのサイトの一つの目的。|]

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
    " I can be reached at " *> a_ [href_ "mailto:colin@fosskers.ca"] "colin@fosskers.ca"

meJ :: Html ()
meJ = do
  h4_ "自分について"
  p_ $ do
    "僕の日本語の旅："
    ul_ $ do
      li_ "2006年・市の日本文化センターで週に3時日本語を勉強し始め"
      li_ "2000年代後半・「Maximum7」というファングループでBLEACHの漫画を翻訳"
      li_ "2008年から2009年・佐賀国立大学で短期留学"
      li_ "2010年から2013年・長崎県佐世保市で英語教師として務め"
      li_ "2011年・日本語能力試験の「N1」を獲得"
      li_ "2013年・漢検の準二級を獲得"
    toHtml [lt|西日本の方言が好みで、標準語より佐世保弁を話す。
              現在カナダでプログラマーとして働きよっとばい！|]
