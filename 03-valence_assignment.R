library(fancyr)
library(psych)

valenceDataframe <- data.frame(pseudoId=character(0),vaderValence=numeric(0),nrcValence=numeric(0),anewValence=numeric(0))

vaderDict <- read_tsv("data/vader_lexicon.txt", col_names = F)
nrcDict <- read_tsv("data/nrcvad_lexicon.txt", col_names = F)
anewDict <- read_csv("data/anew_lexicon.csv")

for(k in 1:nrow(parsedData)){
  nextLemma <- parsedData$lemma[k]
  
  vaderValence <- as.data.frame(vaderDict[which(vaderDict$X1 == nextLemma),2]) #-4 to 4
  nrcValence <- as.data.frame(nrcDict[which(nrcDict$X1 == nextLemma),2]) #-1 to 1
  anewValence <- as.data.frame(anewDict[which(anewDict$Word == nextLemma),3]) #1 to 9
  
  if(nrow(vaderValence)>0){
    vaderValence <- pomp(vaderValence[1,1],-4,4)
  } else{
    vaderValence <- NA
  }
  
  if(nrow(nrcValence)>0){
    nrcValence <- pomp(nrcValence[1,1],-1,1)
  } else{
    nrcValence <- NA
  }
  
  if(nrow(anewValence)>0){
    anewValence <- pomp(anewValence[1,1],1,9)
  } else{
    anewValence <- NA
  }
  
  valenceDataframe <- rbind(valenceDataframe, data.frame(pseudoId=paste0(parsedData$itemID[k],"_",parsedData$lemma[k]),
                                                         vaderValence=vaderValence,
                                                         nrcValence=nrcValence,
                                                         anewValence=anewValence))
  
}

parsedDataValence <- merge(parsedData,valenceDataframe,by = "pseudoId")
parsedDataValence$avgValence <- rowMeans(parsedDataValence[,13:15],na.rm = T)

parsedDataValence.agg <- group_by(parsedDataValence, itemID) %>% 
  summarise(isNegated=max(negation), vaderValence=mean(vaderValence,na.rm = T), nrcValence=mean(nrcValence,na.rm = T), anewValence=mean(anewValence,na.rm = T), overallAvgValence=mean(avgValence,na.rm = T),isInformallyNegated=max(informalNegation),isReverseCoded=max(reverseCoded))

parsedDataValence.agg$avgValence <- rowMeans(parsedDataValence.agg[,3:5],na.rm = T)

parsedDataValence.agg$avgValenceWithNegations <- parsedDataValence.agg$avgValence
parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$isNegated==1)] <- 1-parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$isNegated==1)]


parsedDataValence.agg$avgValenceWithInformalNegations <- parsedDataValence.agg$avgValence
parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1)] <- 1-parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1)]

parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes <- parsedDataValence.agg$avgValence
parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1)] <- 1-parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1)]
parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which((parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1) & (parsedDataValence.agg$isReverseCoded==1))] <- 1-parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which((parsedDataValence.agg$isNegated==1 | parsedDataValence.agg$isInformallyNegated==1) & (parsedDataValence.agg$isReverseCoded==1))]

write_csv(parsedDataValence.agg,"outputs/item-valence.csv")
