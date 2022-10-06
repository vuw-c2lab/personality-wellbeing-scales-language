baselineSD <- sd(parsedDataValence$avgValence,na.rm = T)
baselineMean <- mean(parsedDataValence$avgValence,na.rm = T)

# item-by-item matrix 1 - difference between avg valence

valDiffMatrixNoNgeations <- matrix(0,ncol=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))),nrow=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))))
colnames(valDiffMatrixNoNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]
rownames(valDiffMatrixNoNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]

valDiffMatrixNgeations <- matrix(0,ncol=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))),nrow=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))))
colnames(valDiffMatrixNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]
rownames(valDiffMatrixNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]

valDiffMatrixInformalNgeations <- matrix(0,ncol=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))),nrow=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))))
colnames(valDiffMatrixInformalNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]
rownames(valDiffMatrixInformalNgeations) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]

valDiffMatrixInformalNgeationsAndReverseCodes <- matrix(0,ncol=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))),nrow=nrow(parsedDataValence.agg)-length(which(is.na(parsedDataValence.agg$avgValence))))
colnames(valDiffMatrixInformalNgeationsAndReverseCodes) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]
rownames(valDiffMatrixInformalNgeationsAndReverseCodes) <- parsedDataValence.agg$itemID[which(!is.na(parsedDataValence.agg$avgValence))]

for(i in 1:(nrow(valDiffMatrixNoNgeations)-1)){
  for(j in (i+1):nrow(valDiffMatrixNoNgeations)){
    valDiffMatrixNoNgeations[i,j] <- abs((parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNoNgeations)[i])]-parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNoNgeations)[j])])/baselineSD)
    valDiffMatrixNoNgeations[j,i] <- abs((parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNoNgeations)[i])]-parsedDataValence.agg$avgValence[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNoNgeations)[j])])/baselineSD)
    
    valDiffMatrixNgeations[i,j] <- abs((parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNgeations)[i])]-parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNgeations)[j])])/baselineSD)
    valDiffMatrixNgeations[j,i] <- abs((parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNgeations)[i])]-parsedDataValence.agg$avgValenceWithNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixNgeations)[j])])/baselineSD)
    
    valDiffMatrixInformalNgeations[i,j] <- abs((parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeations)[i])]-parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeations)[j])])/baselineSD)
    valDiffMatrixInformalNgeations[j,i] <- abs((parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeations)[i])]-parsedDataValence.agg$avgValenceWithInformalNegations[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeations)[j])])/baselineSD)
    
    valDiffMatrixInformalNgeationsAndReverseCodes[i,j] <- abs((parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeationsAndReverseCodes)[i])]-parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeationsAndReverseCodes)[j])])/baselineSD)
    valDiffMatrixInformalNgeationsAndReverseCodes[j,i] <- abs((parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeationsAndReverseCodes)[i])]-parsedDataValence.agg$avgValenceWithInformalNegationsAndReverseCodes[which(parsedDataValence.agg$itemID==colnames(valDiffMatrixInformalNgeationsAndReverseCodes)[j])])/baselineSD)
  }
}

# item-by-item matrix 2 - term set similarity
setSimMatrix <- matrix(0,ncol=nrow(parsedDataValence.agg),nrow=nrow(parsedDataValence.agg))
colnames(setSimMatrix) <- parsedDataValence.agg$itemID
rownames(setSimMatrix) <- parsedDataValence.agg$itemID

for(i in 1:(nrow(setSimMatrix)-1)){
  for(j in (i+1):nrow(setSimMatrix)){
    its1 <- itemTermSets.lists[[i]]
    its2 <- itemTermSets.lists[[j]]
    
    setSimMatrix[i,j] <- length(intersect(its1,its2))
  }
}

# library(foreach)
# library(doParallel)
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)

# item-by-item matrix 3 - term set similarity with synonyms
setSimMatrixSynonyms <- matrix(0,ncol=nrow(parsedDataValence.agg),nrow=nrow(parsedDataValence.agg))
colnames(setSimMatrixSynonyms) <- parsedDataValence.agg$itemID
rownames(setSimMatrixSynonyms) <- parsedDataValence.agg$itemID

# foreach(i=1:(nrow(setSimMatrixSynonyms)-1)) %dopar% {
#   for(j in (i+1):nrow(setSimMatrixSynonyms)){
#     its1 <- itemTermSets.lists[[i]]
#     its2 <- itemTermSets.lists[[j]]
#     
#     itemId1 <- colnames(setSimMatrixSynonyms)[i]
#     itemId2 <- colnames(setSimMatrixSynonyms)[j]
#     
#     item1Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId1),]))$expandedTerm
#     item2Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId2),]))$expandedTerm
#     
#     item1Syns <- c(its1,item1Syns)
#     item2Syns <- c(its2,item2Syns)
#     
#     setSimMatrixSynonyms[i,j] <<- length(intersect(item1Syns,item2Syns))
#     setSimMatrixSynonyms[j,i] <<- length(intersect(item1Syns,item2Syns))
#     sink("Report.txt", append=TRUE)
#     cat(j)
#   }
# }

# finalMatrix <- foreach(i=1:nrow(setSimMatrixSynonyms), .combine=rbind) %dopar% {
#   tempRow <- rep(0,nrow(setSimMatrixSynonyms))
#   for(j in 1:nrow(setSimMatrixSynonyms)){
#     if(i!=j){
#       its1 <- itemTermSets.lists[[i]]
#       its2 <- itemTermSets.lists[[j]]
# 
#       itemId1 <- colnames(setSimMatrixSynonyms)[i]
#       itemId2 <- colnames(setSimMatrixSynonyms)[j]
# 
#       item1Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId1),]))$expandedTerm
#       item2Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId2),]))$expandedTerm
# 
#       item1Syns <- c(its1,item1Syns)
#       item2Syns <- c(its2,item2Syns)
# 
#       tempRow[j] <- length(intersect(item1Syns,item2Syns))
#       
#       cat(paste0(i,"-",j," ## "), file="log.txt", append=TRUE)
#     }
#   }
#   tempRow
# }
# 
# 
# stopCluster(cl)
# 
# # item-by-item matrix 4 - term set similarity with synonyms, removed items with negation and informal negation
# 
# #which items have negations
# parsedDataValence.agg.noNegations <- parsedDataValence.agg[which(parsedDataValence.agg$isNegated+parsedDataValence.agg$isInformallyNegated==0),]
# 
# setSimMatrixSynonymsNoNegations <- matrix(0,ncol=nrow(parsedDataValence.agg.noNegations),nrow=nrow(parsedDataValence.agg.noNegations))
# colnames(setSimMatrixSynonymsNoNegations) <- parsedDataValence.agg.noNegations$itemID
# rownames(setSimMatrixSynonymsNoNegations) <- parsedDataValence.agg.noNegations$itemID
# 
# finalMatrix <- foreach(i=1:nrow(setSimMatrixSynonymsNoNegations), .combine=rbind) %dopar% {
#   tempRow <- rep(0,nrow(setSimMatrixSynonymsNoNegations))
#   for(j in 1:nrow(setSimMatrixSynonymsNoNegations)){
#     if(i!=j){
#       its1 <- itemTermSets.lists[[i]]
#       its2 <- itemTermSets.lists[[j]]
#       
#       itemId1 <- colnames(setSimMatrixSynonymsNoNegations)[i]
#       itemId2 <- colnames(setSimMatrixSynonymsNoNegations)[j]
#       
#       item1Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId1),]))$expandedTerm
#       item2Syns <- dplyr::filter(wordnetExpansion.noAntonyms,termID %in% rownames(parsedData[which(parsedData$itemID==itemId2),]))$expandedTerm
#       
#       item1Syns <- c(its1,item1Syns)
#       item2Syns <- c(its2,item2Syns)
#       
#       tempRow[j] <- length(intersect(item1Syns,item2Syns))
#     }
#   }
#   tempRow
# }
