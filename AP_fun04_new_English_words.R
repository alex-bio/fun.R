## Make new English words

## starting consonant and vowel libraries
consonant.lib <- c("b","c","d","f","g","h","j","k","l","m","n","p","qu","r","s","t","v","w","x","y","z")
vowel.lib <- c("a","e","i","o","u","y")
## write a function for this
word <- function() {
  ## distribution of word lengths in English from Lord Rothschild 1985 Journal of Stat Planning and Inference 1986 (Fig. 3) is shifted Poisson, mean  = 6.94, var = 5.80, which we will approximate with Poisson lambda = 6.94. 
  nums <- rpois(1,6.94)
  ## let's just keep on approximating
  ## maybe secretly engineer
  ## divide by 2 since this builds words by "zipping" together alternating consonants and vowels.
  consonants <- sample(consonant.lib, size=nums/(2),replace=T)
  vowels <- sample(vowel.lib, size=nums/(2),replace=T)
  
  true.length <- sum(length(consonants),length(vowels))
  ## length has to be sum since nums/2 is rounded down when nums is odd
  odds <- seq(from=1,to=true.length,by=2)
  evens <-seq(from=2,to=true.length,by=2)
  ## assign odds/evens to consonants/vowels respectively in order to "zipper" them together to paste collapse into a final word
  df1 <- data.frame(variable = c(consonants,vowels), value = c(odds,evens))
  df1 
  ## now reorder by column "value" and unlist
  df1 <- df1[order(df1$value),]
  return(paste(unlist(df1$variable),sep="", collapse=""))
  ## wow
  ## has science gone too far
}
word()
