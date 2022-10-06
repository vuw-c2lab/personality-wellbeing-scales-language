# wordnet
library(wordnet)
#Sys.setenv(WNHOME = "/usr/local/Cellar/wordnet/3.1_1/")
#setDict("/usr/local/Cellar/wordnet/3.1_1")

wordnetExpansion <- data.frame(termID=numeric(0),expandedTerm=character(0),synonym_antonym=character(0))

for(j in 1:nrow(parsedData)){
  term <- parsedData$lemma[j]
  if(parsedData$posType[j]=="ADJ"){
    termType <- "ADJECTIVE"
  } else if(parsedData$posType[j]=="NOUN" | parsedData$posType[j]=="PROPN"){
    termType <- "NOUN"
  } else if(parsedData$posType[j]=="VERB"){
    termType <- "VERB"
  }
  
  filter <- getTermFilter("ExactMatchFilter", term, TRUE)
  terms <- getIndexTerms(termType, 5, filter)
  
  #synonyms
  if(!is.null(terms)){
    syns <- synonyms(term,termType)
    syns <- syns[which(nchar(syns)>2 & !(syns %in% term))]
  }
  
  for(syno in syns){
    wordnetExpansion <- rbind(wordnetExpansion,
                              data.frame(termID=j,expandedTerm=syno,synonym_antonym="synonym"))
  }
  
  #antonyms
  synsets <- tryCatch(
    getSynsets(terms[[1]]),
    error = function(condition) {
      message("No direct antonym found")
      if (condition$message == "RcallMethod: invalid object parameter")
        message("No synset created")
      else
        stop(condition)
      return(NULL)
    }
  )
  if(!is.null(synsets)){
    related <- tryCatch(
      getRelatedSynsets(synsets[[1]], "!"),
      error = function(condition) {
        message("No direct antonym found")
        if (condition$message == "RcallMethod: invalid object parameter")
          message("No direct antonym found")
        else
          stop(condition)
        return(NULL)
      }
    )
    if(!is.null(related)){
      antos <- sapply(related, getWord)
      for(anto in antos){
        wordnetExpansion <- rbind(wordnetExpansion,
                                  data.frame(termID=j,expandedTerm=anto,synonym_antonym="antonym"))
      }
    }
  }
}

wordnetExpansion <- unique(wordnetExpansion)
