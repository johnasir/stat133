# Midterm 3

# Write a function called numBracElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "[" symbol
#
# and return the following
#   <num.brac>: an integer indicating how many elements of <chvec> contain the "["
#     symbol. For example: numBracElements(c('digit', '[:digit:]', '[]')) should return 2

numBracElements = function(chvec) {
  count  = 0;
  for (i in 1:length(chvec)) {
    
    x = unlist(strsplit(chvec[i], ""))
    for (j in 1:length(x)) {
      if (x[j] == "[") {
        count = count + 1
        break
      }
      
    }
  }
  return (count)
}


# Write a function called maxDigits that return the maximum of all (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the maximum of all digits in chvec)
maxDigits = function(chvec) {
  if (grepl("9", chvec)) {
    return (9)
  } else if (grepl("8", chvec)) {
    return (8)
  } else if (grepl("7", chvec)) {
    return (7)
  } else if (grepl("6", chvec)) {
    return (6)
  } else if (grepl("5", chvec)) {
    return (5)
  } else if (grepl("4", chvec)) {
    return (4)
  } else if (grepl("3", chvec)) {
    return (3)
  } else if (grepl("2", chvec)) {
    return (2)
  } else if (grepl("1", chvec)) {
    return (1)
  } else {
    return (0)
  }
}



# Some test cases:
# all.equal(maxDigits("1z3p ! 28"), 8)
# all.equal(maxDigits("abcdefg"), 0)



# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to her. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the equired substitutions.
hisToHer = function(chvec) {
  x  = unlist(strsplit(chvec, " "))
  x[1] = gsub("his", "her", x[1])
  x[1] = gsub("him", "her", x[1])
  x[1] = gsub("he", "she", x[1])
  x[length(x)] = gsub("his", "her", x[length(x)])
  x[length(x)] = gsub("him", "her", x[length(x)])
  x[length(x)] = gsub("he", "she", x[length(x)])
  herchvec = paste(x, collapse = " ")
  herchvec = gsub(" his ", " her ", herchvec)
  herchvec = gsub(" him ", " her ", herchvec)
  herchvec = gsub(" he ", " she ", herchvec)

  return(herchvec)
}

# A test case
all.equal(
  hisToHer("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)


# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

