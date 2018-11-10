Kanji
====

`kanji` is a Japanese Kanji library and analysis program written in Haskell. 
`kanji` can output the Level of the Japanese National Kanji Examination (漢字検定) 
a given Kanji character belongs in.

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
Usage: kanji [-d|--density] [-e|--elementary] [-l|--leveldist] [-s|--splits]
             ((-f|--file ARG) | JAPANESE)

Available options:
  -h,--help                Show this help text
  -d,--density             Find how much of the input is made of Kanji
  -e,--elementary          Find density of Kanji learnt in elementary school
  -l,--leveldist           Find the distribution of Kanji levels
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
$> kanji -sled これは日本語。串と糞
{
    "levelSplit": {
        "Nine": ["語"],
        "Ten": ["本", "日"],
        "Unknown": ["糞"],
        "Two": ["串"]
    },
    "elementary": 0.6,
    "density": 0.5,
    "distributions": {
        "Nine": 0.2,
        "Ten": 0.4,
        "Unknown": 0.2,
        "Two": 0.2
    }
}
```
