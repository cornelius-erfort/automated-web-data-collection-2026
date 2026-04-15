# unscrambler("Qe. Naa-Xevfgva Fgratre")

unscrambler <- function (x) {
  unscramble <- c(
    "a" = "n",
    "A" = "N",
    "b" = "o",
    "B" = "O",
    "c" = "p",
    "C" = "P",
    "d" = "q",
    "D" = "Q",
    "e" = "r",
    "E" = "R",
    "f" = "s",
    "F" = "S",
    "g" = "t",
    "G" = "T",
    "h" = "u",
    "H" = "U",
    "i" = "v",
    "I" = "V",
    "j" = "w",
    "J" = "W",
    "k" = "x",
    "K" = "X",
    "l" = "y",
    "L" = "Y",
    "m" = "z",
    "M" = "Z",
    "N" = "A",
    "n" = "a",
    "o" = "b",
    "O" = "B",
    "p" = "c",
    "P" = "C",
    "q" = "d",
    "Q" = "D",
    "r" = "e",
    "R" = "E",
    "s" = "f",
    "S" = "F",
    "t" = "g",
    "T" = "G",
    "u" = "h",
    "U" = "H",
    "v" = "i",
    "V" = "I",
    "w" = "j",
    "W" = "J",
    "X" = "K",
    "x" = "k",
    "y" = "l",
    "Y" = "L",
    "z" = "m",
    "Z" = "M"
  )
  for(i in 1:str_length(x)) {
    thischar <- unscramble[substr(x, i, i)]
    if(is.na(thischar)) thischar <- substr(x, i, i)
    if(i == 1) {
      result <- thischar
    } else {
      result <- str_c(result, thischar)
    }
    
  }
  result
}

