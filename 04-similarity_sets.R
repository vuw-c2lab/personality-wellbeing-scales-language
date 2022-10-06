itemTermSets <- group_by(parsedData, itemID) %>% summarise(termSet=paste(sort(lemma),collapse = ", "))

itemTermSets.lists <- lapply(itemTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})

itemTermSets.lists <- lapply(itemTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})

facetInstrumentTermSets <- group_by(parsedData, instrument, facet) %>% summarise(termSet=paste(sort(lemma),collapse = ", "))

facetInstrumentTermSets.lists <- lapply(facetInstrumentTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})

domainInstrumentTermSets <- group_by(parsedData, instrument, domain) %>% summarise(termSet=paste(sort(lemma),collapse = ", "))

domainInstrumentTermSets.lists <- lapply(domainInstrumentTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})

facetTypeTermSets <- group_by(parsedData, instrumentType, facet) %>% summarise(termSet=paste(sort(lemma),collapse = ", "))

facetTypeTermSets.lists <- lapply(domainTypeTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})

domainTypeTermSets <- group_by(parsedData, instrumentType, domain) %>% summarise(termSet=paste(sort(lemma),collapse = ", "))

domainTypeTermSets.lists <- lapply(domainTypeTermSets$termSet,function(x){
  unique(tolower(strsplit(x,", ")[[1]]))
})
