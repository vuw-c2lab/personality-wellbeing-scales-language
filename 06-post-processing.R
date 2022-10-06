library(future)

baselineSD <- sd(parsedDataValence$avgValence,na.rm = T)
baselineMean <- mean(parsedDataValence$avgValence,na.rm = T)

wordnetExpansion.noAntonyms <- wordnetExpansion[which(wordnetExpansion$synonym_antonym=="synonym"),]

completeAnalysisDf <- data.frame(item1=character(0),
                                 item2=character(0),
                                 instrument1=character(0),
                                 instrument2=character(0),
                                 domain1=character(0),
                                 domain2=character(0),
                                 facet1=character(0),
                                 facet2=character(0),
                                 itemText1=character(0),
                                 itemText2=character(0),
                                 rawValenceDifference=numeric(0),
                                 withFormalNegationsValenceDifference=numeric(0),
                                 withInformalNegationsValenceDifference=numeric(0),
                                 withReverseCodesValenceDifference=numeric(0),
                                 termSetSimilarity=numeric(0),
                                 termSetSimilarityWithSynonyms=numeric(0),
                                 termSetSimilarityWithSynonymsRemovedItemsWithBothNegations=numeric(0))

setSimMatrixSynonyms <- matrix(0,ncol=nrow(parsedDataValence.agg),nrow=nrow(parsedDataValence.agg))
colnames(setSimMatrixSynonyms) <- parsedDataValence.agg$itemID
rownames(setSimMatrixSynonyms) <- parsedDataValence.agg$itemID
nums <- seq.int(1,nrow(setSimMatrixSynonyms),1)
Cmb1 <- combn(nums, 2)
lapplyIndex <- 1:ncol(Cmb1)
#cnt <- 1
library("future.apply")
library(foreach)
library(doParallel)
cores=detectCores()
plan(multisession, workers = cores[1]-1)
dataList <- future_lapply(lapplyIndex,function(x){
  cat(paste0(x," ## \n"), file="log.txt", append=TRUE)
  #cnt<<-cnt+1
  
  itemID1 <- parsedDataValence.agg$itemID[Cmb1[1,x]]
  itemID2 <- parsedDataValence.agg$itemID[Cmb1[2,x]]
  
  parsedDataRow1 <- dplyr::filter(parsedData,itemID == itemID1)
  parsedDataRow2 <- dplyr::filter(parsedData,itemID == itemID2)
  
  valenceDataRow1 <- dplyr::filter(parsedDataValence.agg,itemID == itemID1)
  valenceDataRow2 <- dplyr::filter(parsedDataValence.agg,itemID == itemID2)
  
  its1 <- itemTermSets.lists[[which(parsedDataValence.agg$itemID==itemID1)]]
  its2 <- itemTermSets.lists[[which(parsedDataValence.agg$itemID==itemID2)]]
  
  item1Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemID1),]))$expandedTerm
  item2Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemID2),]))$expandedTerm
  
  item1Syns <- c(its1,item1Syns)
  item2Syns <- c(its2,item2Syns)
  
  tempDf <- data.frame(item1=itemID1,
                       item2=itemID2,
                       instrument1=unique(parsedDataRow1$instrument),
                       instrument2=unique(parsedDataRow2$instrument),
                       domain1=unique(parsedDataRow1$domain),
                       domain2=unique(parsedDataRow2$domain),
                       facet1=unique(parsedDataRow1$facet),
                       facet2=unique(parsedDataRow2$facet),
                       rawValenceDifference=abs((parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==itemID1)]-parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==itemID2)])/baselineSD),
                       withFormalNegationsValenceDifference=abs((parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==itemID1)]-parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==itemID2)])/baselineSD),
                       withInformalNegationsValenceDifference=abs((parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==itemID1)]-parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==itemID2)])/baselineSD),
                       withReverseCodesValenceDifference=abs((parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==itemID1)]-parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==itemID2)])/baselineSD),
                       termSetSimilarity=length(intersect(its1,its2)),
                       termSetSimilarityWithSynonyms=length(intersect(item1Syns,item2Syns)),
                       termSetSimilarityWithSynonymsRemovedItemsWithBothNegations=NA)
})

matrix_result <- do.call(rbind,dataList)
write_csv(matrix_result,"outputs/item-item-data.csv")
