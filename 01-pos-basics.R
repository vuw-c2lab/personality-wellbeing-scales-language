library(tidyverse)
library(readxl)
library(spacyr)
library(tidytext)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

spacy_initialize()

sheets <- c("BHI", "HEX", "BFI2", "NEO_IPIP", "NEO", "EP", "B6", "B2", "MPQ_IPIP", "16PF_IPIP", "MBTI", "B5A_IPIP", "OHQ", "RSES", "SWLS", "LOT", "PANAS", "BDI")
negators <- c("rarely", "seldom", "never", "seldomly", "few", "little", "less", "hardly")

wellbeing <- c("OHQ", "RSES", "SWLS", "LOT", "PANAS", "BDI")
personality<- c("BHI", "HEX", "BFI2", "NEO_IPIP", "NEO", "EP", "B6", "B2", "MPQ_IPIP", "16PF_IPIP", "MBTI", "B5A_IPIP")

parsedData <- data.frame(instrument=character(0),
                         domain=character(0),
                         facet=character(0),
                         itemID=character(0),
                         term=character(0),
                         lemma=character(0),
                         posType=character(0),
                         negation=logical(0),
                         informalNegation=logical(0),
                         reverseCoded=logical(0))

for(sheet in sheets){
  Items <- read_excel("data/Items.xlsx", sheet = sheet)
  
  for(itemNo in 1:nrow(Items)){
    parsedItems <- spacy_parse(Items$Item[itemNo], entity = T, nounphrase = T, type = "all", tag = T, dependency = T)
    
    parsedItems.VAN <- parsedItems %>% unnest_tokens(word, token) %>% dplyr::filter(pos == "VERB" | tag == "NN" | tag == "NNS" | tag == "NNP" | tag == "JJ")
    #parsedItems.adjectives <- parsedItems %>% unnest_tokens(word, token) %>% dplyr::filter(tag == "JJ")
    #parsedItems.nouns <- parsedItems %>% unnest_tokens(word, token) %>% dplyr::filter(tag == "NN" | tag == "NNS")
    if(nrow(parsedItems.VAN) > 0){
      #negations???
      getNeg <- parsedItems %>% unnest_tokens(word, token) %>% dplyr::filter(dep_rel == "neg")
      if(nrow(getNeg) > 0 & !is.null(getNeg)){
        getNeg <- getNeg$head_token_id
      } else{
        getNeg <- c()
      }
      
      getInformalNeg <- parsedItems[which(parsedItems$lemma %in% negators),]
      if(nrow(getInformalNeg) > 0 ){
        getInformalNeg <- getInformalNeg$head_token_id
      } else{
        getInformalNeg <- c()
      }
      
      for(i in 1:nrow(parsedItems.VAN)){
        isNegated <- F
        nextTerm <- parsedItems.VAN[i,]
        if(nextTerm$token_id %in% getNeg){
          isNegated <- T
        }
        
        isInformallyNegated <- F
        nextTerm <- parsedItems.VAN[i,]
        if(nextTerm$token_id %in% getInformalNeg){
          isInformallyNegated <- T
        }
        
        reverseCoded <- Items$Reversed[itemNo] == "TRUE"
        
        parsedData <- rbind(parsedData,
                            data.frame(instrument=sheet,
                                       domain=Items$Domains[itemNo],
                                       facet=Items$Facets[itemNo],
                                       itemID=Items$No.[itemNo],
                                       term=nextTerm$word,
                                       lemma=nextTerm$lemma,
                                       posType=nextTerm$pos,
                                       negation=isNegated,
                                       informalNegation=isInformallyNegated,
                                       reverseCoded=reverseCoded))
      }
    }
  }
}


parsedData$pseudoId <- paste0(parsedData$itemID,"_",parsedData$lemma)

parsedData$instrumentType <- "personality"
parsedData$instrumentType[which(parsedData$instrument %in% wellbeing)] <- "wellbeing"

write_csv(parsedData,"outputs/item-data.csv")
