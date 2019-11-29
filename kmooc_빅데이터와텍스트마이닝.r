library(stringr)
fns <- c("fan", "fen", "fin", "fon", "fun")
unlist(str_extract_all(fns, "f[aeiu]n"))

smile <- c("^^;;")
unlist(str_extract_all(smile, "\\^\\^\\;\\;"))

today <- c("Today is April 26th in 2018!")       
unlist(str_extract_all(today, "[^[:punct:][:space:]]+"))
