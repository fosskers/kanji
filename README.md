NANQ
====
Author:  Colin Woodbury <colingw@gmail.com>

NanQ is a Japanese Kanji analysation program written in Haskell. Its main
function is to tell what Kanji belong to what Level of the Japanese National
Kanji Examination (漢字検定).

NanQ can be used to:
 - determine what Level individual Kanji belong to
 - determine the average Level (difficulty, in other words)
   of a group of Kanji
 - apply the above to whole files of Japanese
 - display the results of all the above in Japanese or English

INSTALLING NANQ
---------------
First, get the source files from:

https://github.com/fosskers/nanq

NanQ is written in Haskell and uses the
[stack](http://docs.haskellstack.org/en/stable/README.html) tool. Once
`stack` is installed, move to the source directory and perform:

    stack build
    stack install

USAGE
-----
Assuming you've made it so that you can run the executable, the following
command-line options are available:

#### ANALYSIS OPTIONS
 *no option*      Takes a line of Japanese straight from the
                  command line and gives the Level of each Kanji.

 -a, --average    Given Japanese input, finds the average Level of
                  all Kanji present.
 -u, --unknowns   Reports Kanji whose Level could not be determined.

 -h, --help       Prints a message explaining these options.

 -d, --leveldist  Find the % distribution of Levels in given Japanese.

 -k, --density    Determines how much of the input is made up of Kanji.

 -e, --elementary Determines how much of the input is made up of Kanji
                  leared in Japanese Elementary School.

 -q, --fromq      Filters out all but Kanji from the requested Level.

 -t, --text       Applies -k -e and -d all at once to analyse some text.

#### INPUT SOURCE OPTIONS
 -f, --file       Get input from a given file.

 -p, --pipe       Get input from stdin.

#### OUTPUT LANGUAGE OPTIONS
 -j, --japanese   Gives output in Japanese where applicable.

#### NOTES ON CLOs
 * Either `-f` or `-p` can be used on top of any other option.
 * `-j` will change output to Japanese and can be combined with any option.
 * `-h` will over-ride any other options or arguments, discarding them and
   printing a help message.
 * While the flags in something like:
       nanq -jfk proofOfWhoKilledJFKInJapanese.txt
   can be fused together and can come in any order, when using `-q`
   it's best to separate it and its argument from other flags like so:
       nanq -q 3 -f someFile.txt
 * `-t` is probably the most convenient option for giving a quick
   analysis of some Japanese.

#### Examples
*Single Kanji*
```
nanq 日
>>> 日 is a Tenth Level Kanji.
nanq -j 日
>>> 「日」は10級の漢字
```

*A Japanese sentence*
```
nanq これは日本語
>>> 日 is a Tenth Level Kanji.
    本 is a Tenth Level Kanji.
    語 is a Ninth Level Kanji.
```

*Averages*
```
nanq -a この文は難しくない
>>> Average Level: 7.50
nanq -ja 此の文は難儀だと思える
>>> 平均の級：5.60
```

*Reading from a file (and also finding the average)*
```
nanq -fa file.txt
```

*Reading from stdin (and getting output in Japanese)*
```
cat file.txt | nanq -pj
```

*Density Analysis*
```
nanq -k あら、漢字があまりない
>>> Kanji Density: 18.18%

nanq -e 今日は良い天気
>>> Input Kanji is 100.00% Elementary School Kanji.

nanq -ej 蜘蛛が狸と争い、酷い目に遭った
>>> 入力した漢字は「28.57%」小学校で習う漢字。
```

*Targeting Kanji of a Specific Level*
```
nanq -q 9 あの絵は特に気に入ってる。買おうかな・・・ どう思う？
>>> 絵
    買
    思
```
