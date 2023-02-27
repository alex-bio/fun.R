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

## larger libraries if you want
# consonant.lib <- c("b","c","d","f","g","h","j","k","l","m","n","p","qu","r","s","t","v","w","x","y","z","th","sh", "mb","ch","ck","cr","cs","br","bs","lb","rb","ct","nd","dr","ds","wd","fl","mf","lf","fr","rf","fs","ff","sf","ft","gh","gl","ng","gn","gr","rg","gs","gw","mk","nk","rk","sk","ks","wk","lm","ln","lp","pl","sl","ls","ll","lt","wl","zl","mn","rm","ms","sm","mt","rn","sn","ns","nt","wn","pr","rp","ps","sp","pt","wp","xp","rr","rs","rt","tr","rv","vr","wr", "ss", "st", "ts", "sw","ws", "tt","tw","wt","xt","vv","zz","bb","mpt","sch","lpt","rnt","str")

# vowel.lib <- c("a","e","i","o","u",
#            "ai","ao","au",
#            "ee","ea","ei","eo","eu",
#            "ia","ie","io",
#            "oa","oi","ou",
#            "ua","ue","ui","uo")

## average letters in consonant vector element
# avg.cons <- sum(nchar(consonant.lib))/length(consonant.lib)
## average letters in vowel vector element
# avg.vows <- sum(nchar(vowel.lib))/length(vowel.lib)
## these are for later reducing whatever random Poisson value we get to close to accurate length of word
