Binary <- function (df){
  #creates dummy variables for a given data frame
  acm.util.df <- function(i) {
    cl <- df[, i]
    cha <- names(df)[i]
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(row.names(df), paste(cha, levels(cl), 
                                             sep = ""))
    return(x)
  }
  G <- lapply(1:ncol(df), acm.util.df)
  G <- data.frame(G, check.names = FALSE)
  return(G)
}

`%ni%` <- Negate(`%in%`)

is.nan.data.frame <- function(x)    do.call(cbind, lapply(x, is.nan))

Fun_Volume_Split <- function(Xtables,SecondaryWeights,Index){
  ###We split volume to the corresponding secondary's attributes
  WeightTable  <- Xtables
  for(ia in 1:nrow(Xtables)){
    if(ia %% 25 == 0 | ia==nrow(Xtables)){
      print(paste0(substr(CSVs[seg],12,nchar(CSVs[seg])-4),". Phase A. Split volume to each level of attributes : ",ia," of ",nrow(Xtables)))
    }
    Theseis <- which(Xtables[ia,] == 1)
    TempWeightsNames <- colnames(Xtables)[which(Xtables[ia,] == 1)]
    TempWeights <- SecondaryWeights[SecondaryWeights[ ,"Attribute_Level"] %in% TempWeightsNames,c("Attribute_Level","Level_Avg_Weight")]
    TempWeights <- TempWeights[match(TempWeightsNames, TempWeights$Attribute_Level),"Level_Avg_Weight"]
    Paronomastis <- sum(  TempWeights )
    WeightTable[ia,Theseis] <-  Xtables[ia,2] * TempWeights / Paronomastis
  }
  return(WeightTable)
}

Fun_Loss_Split <- function(WeightTable,Index){
  IncrementalTable <- WeightTable
  for(j in Index:ncol(WeightTable)){
    IncrementalTable[,j] <- ((WeightTable[,j] /sum(WeightTable[,j])) ^ 2) * WeightTable[,j]
  }
  IncrementalTable[is.nan(IncrementalTable)] <- 0
  return(IncrementalTable)
}

What_if_split <- function(Remain_Table,WeightTable,Score_Table,Index){
  for(Rem in 1:nrow(Remain_Table) ){
    if(Rem %% 10 == 0 | Rem==nrow(Xtables)){
      print(paste0(substr(CSVs[seg],12,nchar(CSVs[seg])-4),". Distribute Volume to the remaining skus (whatif) : ",Rem," of ",nrow(Remain_Table)))
    }
    Remain_Table <- cbind(WeightTable[which(colnames(WeightTable) %in% c(DepVarName,SKU_name, "manufacturer" ))],WeightTable[which(colnames(WeightTable) %ni% c(DepVarName,SKU_name, "manufacturer" ))] - Loss_Table[which(colnames(Loss_Table) %ni% c(DepVarName,SKU_name, "manufacturer" ))])
    TransTable <-  WeightTable
    Bepensa_share <- 0
    Comp_share <-0 
    Theseis <- which(Remain_Table[Rem,Index:ncol(Remain_Table)] != 0) + Index - 1 
    for(j in Theseis){
      stoixeio <-Remain_Table[Rem,j]
      TransTable[Rem,j] <- 0
      Remain_Table[Rem,j] <- 0
      paronomastis <- sum(TransTable[,j])
      Bepensa_share  <- sum( (TransTable[TransTable[,manufacturer]=="BEPENSA",j]/paronomastis) * stoixeio) + Bepensa_share
      Comp_share <- sum( (TransTable[TransTable[,manufacturer]!="BEPENSA",j]/paronomastis) * stoixeio) + Comp_share
      TransTable[,j] <-  (TransTable[,j] / paronomastis ) * stoixeio + TransTable[,j]
    }
    BEPENSA_pct <- Bepensa_share /( Bepensa_share+Comp_share)
    Comp_pct <- Comp_share /( Bepensa_share+Comp_share)
    Sku <- TransTable[Rem,"Grouping_sku_name_new"]
    Score_Table[ Score_Table[,"Grouping_sku_name_new"]==Sku,"Remaining_BEPENSA"]  <- BEPENSA_pct
    Score_Table[ Score_Table[,"Grouping_sku_name_new"]==Sku,"Remaining_Comp"] <- Comp_pct
    Score_Table[ Score_Table[,"Grouping_sku_name_new"]==Sku,"TransferedToBEPENSA"]  <- Bepensa_share
    Score_Table[ Score_Table[,"Grouping_sku_name_new"]==Sku,"TransferedToComp"]  <- Comp_share
    
  }
  Score_Table$Remaining_Comp <-(Score_Table$TransferedToComp*Score_Table$DenormalizedFactor)/sum(Score_Table[Score_Table[,"manufacturer"]=="BEPENSA","Total_Volume"])
  return(Score_Table)
}