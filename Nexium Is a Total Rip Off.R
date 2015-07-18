setwd("~/Downloads")
library(data.table)
library(ggplot2)

#The fread Function is by far the fastest function beating ff and read.table (which never finished) 
colclassesvector= c("numeric", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "numeric", "numeric")
Drug.data <- fread("PARTD_PRESCRIBER_PUF_NPI_DRUG_13.tab", sep ="\t", verbose=TRUE, colClasses= colclassesvector)

#### SELECT PROTON PUMP INHIBITORS gs.com/drug-class/proton-pump-inhibitors.html?condition_id=&generic=1&sort=rating&order=desc
Segemented.Drug.data <- Drug.data[ GENERIC_NAME ==c("ESOMEPRAZOLE MAGNESIUM", "OMEPRAZOLE", "RABERPRAZOLE","PANTOPRAZOLE", "LANSOPRAZOLE", "DEXLANSOPRAZOLE") ]
remove(Drug.data)
#####Total Sales by Drug Name
Sales.Info<-Segemented.Drug.data[, .(TotalCost=(sum(TOTAL_DRUG_COST)), TotalCostperDay=(sum(TOTAL_DRUG_COST)/sum(TOTAL_DAY_SUPPLY)), Total_Day_Supply = sum(TOTAL_DAY_SUPPLY)) , by= DRUG_NAME]

#####Nexium is 17x the Price
#####Select Nexium and Generic Omeprazoil
Nexium.vs.OMEPRAZOLE <- Segemented.Drug.data[GENERIC_NAME ==c("ESOMEPRAZOLE MAGNESIUM", "OMEPRAZOLE")]

#### by each NPI Number Sum the Total Days Supply of Nexium and OMEPRAZOLE
Nexium.vs.OMEPRAZOLEExtracolum <- Nexium.vs.OMEPRAZOLE[, .(CombinedSupply= sum(TOTAL_DAY_SUPPLY), NPI,  NPPES_PROVIDER_LAST_ORG_NAME,  NPPES_PROVIDER_FIRST_NAME,	NPPES_PROVIDER_CITY,	NPPES_PROVIDER_STATE,	SPECIALTY_DESC, DRUG_NAME, TOTAL_DAY_SUPPLY,  TOTAL_DRUG_COST)   , by =NPI ]
###SelectonlyColums Containing Nexium (or Omeprazole either or)
NexiumOnly <- Nexium.vs.OMEPRAZOLEExtracolum[ DRUG_NAME == "NEXIUM",]
##### Add Colum with the Ratio of Nexium Prescribed (By Days of Prescription) to Nexium+OMEPRAZOLE
NexiumOnly <- NexiumOnly[ , .(NPI,  NPPES_PROVIDER_LAST_ORG_NAME,  NPPES_PROVIDER_FIRST_NAME,  NPPES_PROVIDER_CITY,	NPPES_PROVIDER_STATE,	SPECIALTY_DESC, DRUG_NAME, TOTAL_DAY_SUPPLY, CombinedSupply,TOTAL_DRUG_COST, NEXIUM_RATIO= (TOTAL_DAY_SUPPLY/CombinedSupply)) ]


####CostSavedifTotalyGeneric
Mean.Nexium.Ratio<- NexiumOnly[, mean(NEXIUM_RATIO)]
Total.Sales <- Nexium.vs.OMEPRAZOLE[, sum(TOTAL_DRUG_COST)]
Total.Sales.if.OMEPRAZOLE <- Nexium.vs.OMEPRAZOLE[, sum(TOTAL_DAY_SUPPLY)]*Sales.Info[DRUG_NAME == "OMEPRAZOLE", TotalCostperDay ]
Ratio.of.Reduction.if.OMEPRAZOLE <-Total.Sales.if.OMEPRAZOLE/Total.Sales
Total.Savings.to.Goverment <- Total.Sales- Total.Sales.if.OMEPRAZOLE

#### BrandConcienseDoctors- Proscribe Mostly Nexium
BrandConciousList <- NexiumOnly[ NEXIUM_RATIO >= .95 ]
#### BrandBlindDoctors- Proscribe Mostly OMEPRAZOLE 
BrandBlindList <- NexiumOnly[ NEXIUM_RATIO <= .05 ]
#### Doctors by State
BrandsbyState <- NexiumOnly[ , .(Nexium.Ratio = mean(NEXIUM_RATIO)) , by = NPPES_PROVIDER_STATE  ]
BrandsbyState <- BrandsbyState[order(Nexium.Ratio)]
#####