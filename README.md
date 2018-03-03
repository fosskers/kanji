Kanji
====

`kanji` is a Japanese Kanji library and analysation program written in Haskell. Its main
function is to tell what Kanji belong to what Level of the Japanese National
Kanji Examination (漢字検定).

`kanji` can be used to:
 - determine what Level individual Kanji belong to
 - determine the average Level (difficulty, in other words) of a group of Kanji
 - apply the above to whole files of Japanese

INSTALLING `kanji`
---------------
First, get the source files from:

https://github.com/fosskers/kanji

`kanji` is written in Haskell and uses the
[stack](http://docs.haskellstack.org/en/stable/README.html) tool. Once
`stack` is installed, move to the source directory and perform:

    stack install

USAGE
-----
Assuming you've made it so that you can run the executable, the following
command-line options are available:

```
Usage: kanji [-u|--unknowns] [-d|--density] [-e|--elementary] [-l|--leveldist]
             [-a|--average] [-s|--splits] ((-f|--file ARG) | JAPANESE)

Available options:
  -h,--help                Show this help text
  -u,--unknowns            Find Kanji whose Level couldn't be determined
  -d,--density             Find how much of the input is made of Kanji
  -e,--elementary          Find density of Kanji learnt in elementary school
  -l,--leveldist           Find the distribution of Kanji levels
  -a,--average             Find the average Level of all Kanji present
  -s,--splits              Show which Level each Kanji belongs to
  -f,--file ARG            Take input from a file
```

#### NOTES ON CLOs
 * All options above can be mixed to include their analysis result
 in the output JSON.
 * `-h` will over-ride any other options or arguments, discarding them and
   printing a help message.

#### Examples
*Single Kanji*
```
$> kanji -s 日
{
    "levelSplit": {
        "Ten": [
            "日"
        ]
    }
}
```

*A Japanese sentence*
```
$> kanji -s これは日本語
{
    "levelSplit": {
        "Nine": ["語"],
        "Ten": ["本", "日"]
    }
}
```

*All options*
```
$> kanji -leadus これは日本語
{
    "levelSplit": {
        "Nine": ["語"],
        "Ten": ["本", "日"]
    },
    "elementary": 1,
    "average": 9.666667,
    "density": 0.5,
    "unknowns": [],
    "distributions": {
        "Nine": 0.33333334,
        "Ten": 0.6666667
    }
}
```
