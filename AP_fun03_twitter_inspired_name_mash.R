
## Tweet, @pmddomingos "The ideal tech company would have Apple's design flair, Google's AI and Amazon's operations.
## What would this company be called?
candidate <- c("ap","ple", "goo","gle", "am","a","zon")
name.me <- function(syllables) {
  return(ifelse(syllables <= length(candidate), 
                paste(sample(candidate, size=syllables), collapse=""), 
                paste(sample(candidate, size=syllables, replace=T), collapse=""))
        )
  }
## "Gooplezon", or "Amgoople", or perhaps "Goozongle"?







