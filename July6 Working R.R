setwd("~/Downloads")
library(data.table)

#The fread Function is by far the fastest function beating ff and read.table (which never finished) 
colclassesvector= c("numeric", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "numeric", "numeric")
Drug.data <- fread("PARTD_PRESCRIBER_PUF_NPI_DRUG_13.tab", sep ="\t", verbose=TRUE, colClasses= colclassesvector)
##PBS Cost File
PBS.Data <-read.table("PBSdata.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)
PBS.Data <- data.table(PBS.Data)
#Internal Variation of Drug Price
Subsetted.Drug.Data <- Drug.data[ , .(NPI, DRUG_NAME, NPPES_PROVIDER_LAST_ORG_NAME, NPPES_PROVIDER_FIRST_NAME, SPECIALTY_DESC, TOTAL_CLAIM_COUNT, TOTAL_DAY_SUPPLY, TOTAL_DRUG_COST)]
remove(Drug.data)

#Average Cost by Drug Names
CostbyBrand <- Subsetted.Drug.Data[ , .(TotalCost=(sum(TOTAL_DRUG_COST)), TotalCostperDay=(sum(TOTAL_DRUG_COST)/sum(TOTAL_DAY_SUPPLY)), Total_Day_Supply = sum(TOTAL_DAY_SUPPLY)) , by= DRUG_NAME]
CostbyBrandOrderedperDay <- CostbyBrand[order(-TotalCost)]
remove(CostbyBrand)
OrderedDrugnamelist <- CostbyBrandOrderedperDay[,DRUG_NAME]

#### PBS Comparison Section
###Load Data
PBS.Dataset <-read.csv("July4AMMENDEDPBSfixedData20131201.csv", header=T, sep= ',', stringsAsFactors = FALSE)
PBS.Dataset<- data.table(PBS.Dataset)
####Drugs to Compare to PBS
ListofDrugs <- CostbyBrandOrderedperDay[,DRUG_NAME]
PBSComparison <- data.frame("Drug Name"= character(), "PBSComparisonPrice"= numeric(), "Medicare_per_Unit_Cost"= numeric(), "Total_Day_Supply"= numeric(), stringsAsFactors= FALSE)
### Setkey needed, don't know why but Fuck Up Later Calcs in the extra Variable
InternalCostbyBrandOrderedperDay <- CostbyBrandOrderedperDay
setkey(InternalCostbyBrandOrderedperDay)
####Run Loop
j=1
for (j in 1:50){
  ListofDrugs[j] 
  ### To be Conservative this Function finds the Most Expensive Form (i.e Highest Dosage)
  Result <- PBS.Dataset[ grep(ListofDrugs[j], Trade.Product.Pack.Info, ignore.case=TRUE ), ]
  ### This is because Medicare Pricing Data is an Average so it is a Conservative Assumption
  PriceofLargestDosage <- Result[, max(Cost_per_UOM)]
  newrow <- c(ListofDrugs[j], PriceofLargestDosage, InternalCostbyBrandOrderedperDay[ListofDrugs[j],TotalCostperDay], InternalCostbyBrandOrderedperDay[ListofDrugs[j],Total_Day_Supply])
  rowcounter <- nrow(PBSComparison)+1
  PBSComparison[rowcounter,] <- newrow
}
####PBSComparisonGivesAveragePricesperDay from both Medicare and PBS
####PBS Data is converted to cost per day by assuming that Largest Dose (i.e Dose in a Pill) that Can be Purchased is the Maximum Daily Rate; this is has been found to be True for the Top Ten Sellers by using drugs.com.
####Some Big Sellers were not Found on the PBS
####PBS prices are in AUD; 1USD to 1.03 AUD 
###Make PBSComparison a Data.Table
PBSComparison <- data.table(PBSComparison)
#Intialize Final Report
FinalDrugCostLedger <- data.frame("Drug Name"= character(), "Total Sales"= numeric(), "Total Days Supply"= numeric(), "Cost per Day USA"= numeric(),"Cost per Day Aus"= numeric(), "PercentageofWaste"= numeric() ,stringsAsFactors= FALSE)

# Individual Records of Drugs by Cost
CostperDose <- Subsetted.Drug.Data[,.(NPI, DRUG_NAME, NPPES_PROVIDER_LAST_ORG_NAME, NPPES_PROVIDER_FIRST_NAME, SPECIALTY_DESC, TOTAL_CLAIM_COUNT, TOTAL_DAY_SUPPLY, TOTAL_DRUG_COST, CostperDose= (TOTAL_DRUG_COST/TOTAL_DAY_SUPPLY))]
#For Loop- Returns Drug Level Data by Parsing Every Single Row
i=1
for (i in 1:50){
  Drugname <- ListofDrugs[i]
  ###Annonying Setkey Programming
  key(CostperDose)
  setkey(CostperDose)
  key(CostperDose)
  ###For a Given Drug Returns Cost information for each Subscribing Doctor on a Cost per Dose Basis
  SingleDrugCostperDose <- CostperDose[ DRUG_NAME %in% c(Drugname), .(NPI, DRUG_NAME, NPPES_PROVIDER_LAST_ORG_NAME, NPPES_PROVIDER_FIRST_NAME, SPECIALTY_DESC, TOTAL_CLAIM_COUNT, TOTAL_DAY_SUPPLY, TOTAL_DRUG_COST, CostperDose= (TOTAL_DRUG_COST/TOTAL_DAY_SUPPLY))] 
  ###Total Sales for a Given Drug (A Single Number)
  SingleDrugCostTotalCost <- SingleDrugCostperDose[, sum(TOTAL_DRUG_COST) ]
  #Set Index, maybe Unneccasary
  setkey(SingleDrugCostperDose)
  ### Criteria to Judge Overcharging (Either 3 Standard Deviations or PBS Price)
  OverChargingThreshold <- PBSComparison[ Drug.Name %in% c(Drugname), PBSComparisonPrice ] ###Retrive the SingleDrugCostperDose
  OverChargingThreshold <- as.numeric(OverChargingThreshold)
  #Create a Colum to Test if the Value of SingleDrugCostperDose is Larger than Criteria 
  ListofOverChargers <- SingleDrugCostperDose[, .(NPI, DRUG_NAME, NPPES_PROVIDER_LAST_ORG_NAME, NPPES_PROVIDER_FIRST_NAME, SPECIALTY_DESC, TOTAL_CLAIM_COUNT, TOTAL_DAY_SUPPLY, TOTAL_DRUG_COST, Costperdose= TOTAL_DRUG_COST/TOTAL_DAY_SUPPLY, OverCharging= (CostperDose > OverChargingThreshold))]
  setkey(ListofOverChargers)  #Set Index, maybe Unneccasary
  
  # Exported Values
  Total_Sales<- ListofOverChargers[, mean(Costperdose)]*ListofOverChargers[, sum(TOTAL_DAY_SUPPLY)]
  Total_Days_Supply <- ListofOverChargers[, sum(TOTAL_DAY_SUPPLY)]
  Cost.per.Dose.USA <- ListofOverChargers[, mean(Costperdose)]
  setkey(PBSComparison)
  Cost.per.Dose.Aus <- PBSComparison[Drugname, PBSComparisonPrice]
  PercentageofWastetoSales<- (as.numeric(Total_Days_Supply)* as.numeric(Cost.per.Dose.Aus)/as.numeric(Total_Sales))
  
  #Store Results of All Drugs
  newrow <- data.frame(Drugname, Total_Sales, Total_Days_Supply, Cost.per.Dose.USA, Cost.per.Dose.Aus, PercentageofWastetoSales)
  FinalDrugCostLedger= rbind(FinalDrugCostLedger, newrow)
}

### Keep as data.frame (data.table throws errors), convert to numeric for Further Analysis, back to Datat.Table
FinalDrugCostLedger[,2:4]<- sapply(FinalDrugCostLedger[,2:4], as.numeric) ###Are the Colums to be Converted 
CompleteFinalDrugCostLedger <- FinalDrugCostLedger
sapply(FinalDrugCostLedger, class)
sum(FinalDrugCostLedger[,2])
FinalDrugCostLedger <- data.table(FinalDrugCostLedger)
data.frame(FinalDrugCostLedger)
FinalDrugCostLedger <- FinalDrugCostLedger[ -grep("Inf", PercentageofWastetoSales, ignore.case=TRUE ),]
data.table(FinalDrugCostLedger)
### Analysis
Top50Sales<- FinalDrugCostLedger[, sum(Total_Sales)]
median(FinalDrugCostLedger$PercentageofWaste)
#### ~10 Billion is Wasted out of ~15.9 Billion
library(portfolio)
map.market(id=FinalDrugCostLedger$Drugname, area=FinalDrugCostLedger$Total_Sales, group=FinalDrugCostLedger$Drugname, color=FinalDrugCostLedger$PercentageofWastetoSales, main="FlowingData Map")
