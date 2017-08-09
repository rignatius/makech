#######TRANSFERABILITY

######################################################
###############   Load Libraries  ####################
######################################################
if("gtools" %in% rownames(installed.packages())==FALSE){  install.packages("gtools")}
require(gtools)
options(scipen = 999)
########################################################

rm(list=ls()) #workspace cleaning
# setwd("C:\Users\aldo.sanchez.chavez\Documents\Trasferability")
# WD <- getwd()

# archive reading and creation of output directory
Inputs <- paste0(getwd(),"/Inputs_Transferability")
if (!file.exists(paste0(getwd(),"/Transferability_Outputs"))){
  dir.create(file.path(paste0(getwd(),"/Transferability_Outputs")))
} 
Output <- paste0(getwd(),"/Transferability_Outputs")

# Definitions------------------------------------------------------------------
####Weights
Loss_Weight   <- 0.05
Comp_weight   <- 0.05 
Volume_Weight <- 0.45
Value_Weight  <- 0.45

DepVarName      <- "volume_bottle_per_POS_month_imp"
SKU_name        <- "Grouping_sku_name_new"
manufacturer    <- "manufacturer"
IndexCheck      <- c("volume_bottle_per_POS_month_imp", "Grouping_sku_name_new", "manufacturer")
Time_Vector     <- c(1:10)

# SKU_Criterion <- 10
Volume_Criterion <- 0.10 #1%
# SKU_List_Criterion <- c("","","")
###############   Define Functions  ##################

source('./R/Binary.R')

# data reading
CSVs <-list.files(Inputs, pattern = "DATA_FINAL" , include.dirs = FALSE)

# iteration over each file in the directory
for(seg in 1:length(CSVs)){
  # for(seg in c(1:2,5,6,9:10)){
  # for(seg in 4:4){
  pta <- proc.time()
  
  ###Import Data sets (just reading the three files with dinamic names according to the iteration)
  #1) Initial Data set
  DATA_FINAL    <- read.csv(paste(Inputs,"/",CSVs[seg],sep=""),
                            sep=",",
                            na.strings="NA",
                            stringsAsFactors = F,
                            row.names = 1)
  # line to correct column names in case some name separators were invalid
  DATA_FINAL    <- cbind(DATA_FINAL[which(colnames(DATA_FINAL) %in% c(DepVarName,SKU_name, "Channel") )],
                         data.frame(lapply(DATA_FINAL[which(colnames(DATA_FINAL) %ni% c(DepVarName,SKU_name,"Channel") )],
                                           gsub, pattern = "\\:|\\.| ",
                                           replacement = "_",
                                           fixed=F,perl=T)))
  #2) Secondary Weights
  Level_Weights <- read.csv(paste0(Inputs,"/Final_Level_Weights_",substr(CSVs[seg],12,nchar(CSVs[seg]))),
                            sep=",",na.strings="NA",stringsAsFactors = F)  
  #3) Price 
  Manufac_price          <- read.csv(paste0(Inputs,"/Manu_Price_",substr(CSVs[seg],12,nchar(CSVs[seg]))),
                                     sep=",",na.strings="NA",stringsAsFactors = F) 
  
  DATA_FINAL <- merge(DATA_FINAL,Manufac_price[,c( "Grouping_sku_name_new",manufacturer)])
  
  IndependentVars        <- DATA_FINAL[which(colnames(DATA_FINAL) %ni% IndexCheck)]
  # expansion to dummy vars
  IndependentVars_Binary <- Binary(IndependentVars)
  # drops the column called intercept, if present, 
  IndependentVars_Binary <- IndependentVars_Binary[,which(colnames(IndependentVars_Binary) %ni%  c("(Intercept)"))]
  
  ############# INCREMENTALITY
  # subsets the data in Level_weights in case unexpected variables are present
  SecondaryWeights  <- Level_Weights[,c("Attribute","Attribute_Level","Level_Weight","Level_Avg_Weight")]
  # xtables contains the level_weights plus the dummy vars
  Xtables           <- cbind(  DATA_FINAL[,which(colnames(DATA_FINAL) %in% IndexCheck)] , IndependentVars_Binary)
  Index             <- length (which (colnames(Xtables) %in% IndexCheck)) + 1
  
  ###Create Tables
  WeightTable  <- Fun_Volume_Split(Xtables,SecondaryWeights,Index)
  Loss_Table   <- Fun_Loss_Split(WeightTable,Index)
  Remain_Table <- cbind(WeightTable[which(colnames(WeightTable) %in% IndexCheck)],
                        WeightTable[which(colnames(WeightTable) %ni% IndexCheck)] - Loss_Table[which(colnames(Loss_Table) %ni% IndexCheck)])
  Incrementality <- cbind(Loss_Table[,which(colnames(Loss_Table) %in%  c(SKU_name)),drop=F],
                          apply(Loss_Table[,which(colnames(Loss_Table) %ni%  IndexCheck)], 1, sum))
  colnames(Incrementality) <- c("Grouping_sku_name_new","Incrementality")  
  write.csv(Incrementality,paste0(Output,"/Incrementality_",substr(CSVs[seg],12,nchar(CSVs[seg]))),row.names = F)
  
  ####Create Score Table
  ########Initial Score Table
  Score_Table   <- DATA_FINAL[,which(colnames(DATA_FINAL) %in%  c(SKU_name,DepVarName))]
  Score_Table   <- merge(Score_Table,Incrementality)
  Score_Table   <- merge(Score_Table,
                         Manufac_price[,c( "Grouping_sku_name_new",
                                           "Total_Volume",
                                           "Total_Value",
                                           "avg_number_of_POS_in_stock",
                                           "Avg__No__of_Months_",
                                           "No__of_States",
                                           "manufacturer" )])
  
  Score_Table$DenormalizedFactor <- Score_Table$Avg__No__of_Months_*Score_Table$avg_number_of_POS_in_stock*Score_Table$No__of_States
  Score_Table$Shared_Incrementality <- Score_Table$Incrementality / Score_Table$volume_bottle_per_POS_month_imp
  Score_Table$Share_Volume <- Score_Table$Total_Volume / sum(Score_Table[Score_Table[,"manufacturer"]=="BEPENSA","Total_Volume"])
  Score_Table$Share_Value  <- Score_Table$Total_Value / sum(as.numeric(Score_Table[Score_Table[,"manufacturer"]=="BEPENSA","Total_Value"]))
#  Score_Table$Share_Volume <- Score_Table$Total_Volume / sum(Score_Table$Total_Volume)
#  Score_Table$Share_Value  <- Score_Table$Total_Value / sum(as.numeric(Score_Table$Total_Value))
  Score_Table     <- What_if_split(Remain_Table,WeightTable,Score_Table,Index)
  
  Score_Table$Score <- Volume_Weight * Score_Table$Share_Volume + 
    Loss_Weight * Score_Table$Shared_Incrementality + 
    Value_Weight * Score_Table$Share_Value + 
    Comp_weight * Score_Table$Remaining_Comp

  Report <- data.frame(matrix("NA",ncol = length(unique(Score_Table[,"Grouping_sku_name_new"]))+9,nrow=1))
  colnames(Report) <-c("SKU_NAME",
                       unique(Score_Table[,"Grouping_sku_name_new"]),
                       "Initial_Volume",
                       "PctOfNormVolumeLost",
                       "PctOfNormVolumeTransferred",
                       "PctOfVolumeLost",
                       "PctOfVolumeTransferredComp",
                       "PctOfTotalVolume",
                       "PctOfTotalValue",
                       "CumulativeSUM")
  
  CumulativeSUM  <- 0
  # TotalVolume <- sum(Score_Table[Score_Table[,"manufacturer"]=="BEPENSA","Total_Volume"])
  # Volume_deletion       <- 0
  # Number_of_Droped_Skus <- 0
  
  Score_Table_Initial <- Score_Table
  
  while(  CumulativeSUM < Volume_Criterion  ){
    # while(  Number_of_Droped_Skus < SKU_Criterion  ){
    # Number_of_Droped_Skus <- Number_of_Droped_Skus + 1
    # while(  length(SKU_List_Criterion)>0  ){
    # Sku_To_Delete <- SKU_List_Criterion[1]
    # SKU_List_Criterion <- SKU_List_Criterion[-1]
    
    TempScore <- Score_Table
    TempScore <- Score_Table[which( Score_Table[,manufacturer]=="BEPENSA"),]
    Sku_To_Delete <- TempScore[which(TempScore[,"Score"]==min(TempScore$Score)),SKU_name]
#     Volume_deletion <- Volume_deletion + Score_Table[Score_Table[,SKU_name]== Sku_To_Delete,"Total_Volume"]
    # CumulativeSUM <-  (Volume_deletion/TotalVolume) 
    # write.csv(WeightTable,paste0(Output,"/WeightTable1.csv"),row.names = F)
    # write.csv(Loss_Table,paste0(Output,"/Loss_Table.csv"),row.names = F)
    # write.csv(Remain_Table,paste0(Output,"/Remain_Table.csv"),row.names = F)
    
    Arxiko_volume <- cbind(WeightTable[,which(colnames(WeightTable) %in%  c(SKU_name)),drop=F],
                           apply(WeightTable[,which(colnames(WeightTable) %ni% IndexCheck)], 1, sum))
    colnames(Arxiko_volume) <- c("Grouping_sku_name_new","volume_bottle_per_POS_month_imp_Arxiko")  
    Bepensa_share  <- 0
    Comp_share <- 0
    Rem <- which(Remain_Table[,SKU_name]==Sku_To_Delete)
    for(j in Index :ncol(Remain_Table)){
      if(Remain_Table[Rem,j]==0){
        WeightTable[Rem,j]  <- 0
        next
      }
        stoixeio <-Remain_Table[Rem,j]
        WeightTable[Rem,j]  <- 0
        Remain_Table[Rem,j] <- 0
        paronomastis        <- sum(WeightTable[,j])
        Bepensa_share       <- sum((WeightTable[WeightTable[,manufacturer]=="BEPENSA",j]/paronomastis) * stoixeio) + Bepensa_share
        Comp_share          <- sum((WeightTable[WeightTable[,manufacturer]!="BEPENSA",j]/paronomastis) * stoixeio) + Comp_share
        WeightTable[,j]     <- (WeightTable[,j] / paronomastis ) * stoixeio + WeightTable[,j]
      }
 
    BEPENSA_pct  <- Bepensa_share  /( Bepensa_share+Comp_share)
    Comp_pct <- Comp_share /( Bepensa_share+Comp_share)
    
    #Report Preparation
    NewVolume <- cbind(WeightTable[,which(colnames(WeightTable) %in%  c(SKU_name)),drop=F],
                       apply(WeightTable[,which(colnames(WeightTable) %ni%  IndexCheck)], 1, sum))
    colnames(NewVolume) <- c("Grouping_sku_name_new","volume_bottle_per_POS_month_imp")  
    Volume_Difference <- merge(Arxiko_volume,NewVolume)
    Volume_Difference[,"Transferability"] <- Volume_Difference[,"volume_bottle_per_POS_month_imp"] - Volume_Difference[,"volume_bottle_per_POS_month_imp_Arxiko"]
    Volume_Difference   <- Volume_Difference[Volume_Difference[,"volume_bottle_per_POS_month_imp"]!=0,]
    Volume_Difference_v2   <-t(Volume_Difference[c("Grouping_sku_name_new","Transferability")])
    colnames(Volume_Difference_v2) <- Volume_Difference_v2[1,]
    Volume_Difference_v2 <- as.data.frame(Volume_Difference_v2[-1, ,drop=F]) 
    Volume_Difference_v2[,"SKU_NAME"] <- Sku_To_Delete
    Initial_Volume <- Score_Table[Score_Table[,"Grouping_sku_name_new" ] == Sku_To_Delete,DepVarName]
    
    PctOfNormVolumeTransferred <- (Comp_share + Bepensa_share)/Arxiko_volume[Arxiko_volume[,"Grouping_sku_name_new" ] == Sku_To_Delete,"volume_bottle_per_POS_month_imp_Arxiko"]
    PctOfNormVolumeLost <-  1- PctOfNormVolumeTransferred
    PctOfVolumeLost <- (Score_Table[Score_Table[,"Grouping_sku_name_new" ] == Sku_To_Delete,"DenormalizedFactor"]*
                          sum(Loss_Table[Loss_Table[,"Grouping_sku_name_new" ]== Sku_To_Delete,Index:ncol(Loss_Table)]))/
                          sum(Score_Table_Initial[Score_Table_Initial[,"manufacturer"]=="BEPENSA","Total_Volume"])
    # sum(Volume_Difference$Transferability)
    PctOfVolumeTransferredComp <- (Score_Table[Score_Table[,"Grouping_sku_name_new" ]== Sku_To_Delete,"DenormalizedFactor"]*Comp_share) / 
                          sum(Score_Table_Initial[Score_Table_Initial[,"manufacturer"]=="BEPENSA","Total_Volume"])
    PctOfTotalVolume <-  Score_Table[Score_Table[,"Grouping_sku_name_new" ]== Sku_To_Delete,"Total_Volume"]  / 
                          sum(Score_Table_Initial[Score_Table_Initial[,"manufacturer"]=="BEPENSA","Total_Volume"])
    PctOfTotalValue <-   Score_Table[Score_Table[,"Grouping_sku_name_new" ]== Sku_To_Delete,"Total_Value"] / 
                          sum(as.numeric(Score_Table_Initial[Score_Table_Initial[,"manufacturer"]=="BEPENSA","Total_Value"]))
    CumulativeSUM <- CumulativeSUM + PctOfVolumeLost +PctOfVolumeTransferredComp
    
    DataForReport <- cbind(Volume_Difference_v2,
                           Initial_Volume,
                           PctOfNormVolumeLost,
                           PctOfNormVolumeTransferred,
                           PctOfVolumeLost,
                           PctOfVolumeTransferredComp,
                           PctOfTotalVolume,
                           PctOfTotalValue,
                           CumulativeSUM)
    # write.csv(DataForReport,paste0(Output,"/DataForReport.csv"),row.names = F)
    DataForReport <- sapply(DataForReport,function(x) as.character(x))
    Report <- smartbind(Report,DataForReport)
    
    # Sku Deletion from tables
    WeightTable <- WeightTable[WeightTable[,"Grouping_sku_name_new"]!=Sku_To_Delete,]
    Xtables <-    Xtables[Xtables[,"Grouping_sku_name_new"]!=Sku_To_Delete,]
    Loss_Table  <- Fun_Loss_Split(WeightTable,Index)
    Remain_Table <- cbind(WeightTable[which(colnames(WeightTable) %in% IndexCheck)],
                          WeightTable[which(colnames(WeightTable) %ni% IndexCheck)] - Loss_Table[which(colnames(Loss_Table) %ni% IndexCheck)])
    
    ## Recalculation  of Score
    Score_Table <- Score_Table[-Rem,]
    Score_Table <- Score_Table[,which(colnames(Score_Table) %in%  c("Grouping_sku_name_new"  ,
                                                                    "Share_Volume",
                                                                    "Share_Value",
                                                                    "manufacturer",
                                                                    "Total_Volume",
                                                                    "Total_Value",
                                                                    "DenormalizedFactor" ))]
    Score_Table <- merge(Score_Table,NewVolume)
    Incrementality <- cbind(Loss_Table[,which(colnames(Loss_Table) %in%  c(SKU_name)),drop=F],
                            apply(Loss_Table[,which(colnames(Loss_Table) %ni%  IndexCheck)], 1, sum))
    colnames(Incrementality) <- c("Grouping_sku_name_new","Incrementality")  
    Score_Table <- merge(Score_Table,Incrementality)
    Score_Table$Shared_Incrementality <- Score_Table$Incrementality / Score_Table$volume_bottle_per_POS_month_imp
    Score_Table <- What_if_split(Remain_Table,WeightTable,Score_Table,Index)
    Score_Table$Score <- Volume_Weight * Score_Table$Share_Volume + 
      Loss_Weight * Score_Table$Shared_Incrementality + 
      Value_Weight * Score_Table$Share_Value + 
      Comp_weight * Score_Table$Remaining_Comp
  }
  
  Report2 <- Report[Report[,"SKU_NAME"]!="NA",]
  write.csv(Report2,paste0(Output,"/Report_",substr(CSVs[seg],12,nchar(CSVs[seg]))),row.names = F)
  Time_Vector[seg] <- (proc.time() - pta )[3]
} 