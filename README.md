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
 Short  | Long   | Description
 -----  | ------ | -----------
 -a | --average  | Given Japanese input, finds the average Level of all Kanji present
 -u | --unknowns | Reports Kanji whose Level could not be determined
 -h | --help     | Prints a message explaining these options
 -l | --leveldist | Find the % distribution of Levels in given Japanese
 -s | --splits   | Show what Level each Kanji belongs to
 -d | --density  | Determines how much of the input is made up of Kanji
 -e | --elementary | Determines how much of the input is made up of Kanji learned in Japanese Elementary School

#### INPUT SOURCE OPTIONS
 Short  | Long   | Description
 -----  | ------ | -----------
 *none* | *none* | Analyse a String of Japanese given from the command line
 -f     | --file | Get input from a given file

#### NOTES ON CLOs
 * All options above can be mixed to include their analysis result
 in the output JSON.
 * `-h` will over-ride any other options or arguments, discarding them and
   printing a help message.

#### Examples
*Single Kanji*
```
$> nanq -s 日
{
  "levelSplit": {
    "Ten": "日"
  }
}
```

*A Japanese sentence*
```
$> nanq -s これは日本語
{
    "levelSplit": {
      "Nine": "語",
      "Ten": "本日"
    }
}
```

*All options*
```
$> nanq -leadus これは日本語
{
    "levelSplit": {
      "Nine": "語",
      "Ten": "本日"
    },
    "elementary": 1,
    "average": 9.666667,
    "density": 0.5,
    "unknowns": "",
    "distributions": {
      "Nine": 0.33333334,
      "Ten": 0.6666667
    }
}
```
