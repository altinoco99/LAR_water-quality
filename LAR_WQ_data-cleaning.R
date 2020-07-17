# Anna Tinoco
# 2020-06-15 (Start Date)
# Water quality data cleaning
# Numbers on the end of WQ_## refer to which step of Water quality data cleaning steps was completed to make this version
# Steps 1 & 2 included manually fixing incorrectly input dates and times from the raw data
# and formatting columns to be easily read by R


# load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)

# LAR_WQ_database_01_dates.csv file located in 'C:/Users/altinoco99/Dropbox/LA-River Flows-SCCWRP/04-data/water-quality/databases'
# Use setwd() as appropriate


# The following is commented out as some data was remined and added back in later so code was adapted
#
# Step 03: Load database (and combine like names in Data Source column)
# Step 04: add ID column
# WQ_03 <- read.csv(file = 'LAR_WQ_database_01_dates.csv',
#                stringsAsFactors = FALSE) %>%
#   mutate(ID = str_pad(row_number(), 6, pad = '0'))
# WQ_03$Data.Source[grep('MassEmissions', WQ_03$Data.Source)] <- 'Mass Emissions'
# WQ_03$Data.Source[grep('Watershed_Health', WQ_03$Data.Source)] <- 'Watershed Health'
# 
# WQ_04 <- select(WQ_03, 'ID', 1:(ncol(WQ_03)-1)) %>%
##   filter(Data.Source != 'SMARTS')                     # SMARTs data is cleaned on its own later


# Step 03: Load database (and combine like names in Data Source column)
# Step 03.1: (WQ_03_ms) Remove data for constituents in metals_ss vector from CEDEN and ME
# Step 03.2: (WQ_03_ms_2)Add remined data for metals_ss from CEDEN and Mass Emissions
# Step 04: add ID column
WQ_03_ms <- read.csv(file = 'LAR_WQ_database_01_dates.csv',
                  stringsAsFactors = FALSE) 
WQ_03_ms$Data.Source[grep('MassEmissions', WQ_03_ms$Data.Source)] <- 'Mass Emissions'
WQ_03_ms$Data.Source[grep('Watershed_Health', WQ_03_ms$Data.Source)] <- 'Watershed Health'

metals_ss <- c("Copper, Dissolved", 
               "Copper, Total",
               "Lead, Dissolved",
               "Lead, Total",
               "SpecificConductivity, Total",
               "Suspended Sediment Concentration, Particulate",
               "Total Dissolved Solids, Dissolved",
               "Total Dissolved Solids, Total",
               "Total Suspended Solids, Particulate",
               "Total Suspended Solids, Total",
               "Total Suspended Solids, Volatile",
               "Zinc, Dissolved",
               "Zinc, Total",
               "Copper",
               "Dissolved Copper",
               "Dissolved Lead",
               "Dissolved Zinc",
               "Lead",
               "Specific Conductance",
               "Total Dissolved Solids",
               "Total Suspended Solids",
               "Volatile Suspended Solids",
               "Zinc")

WQ_03_ms_2 <- WQ_03_ms[!((WQ_03_ms$Data.Source == 'CEDEN' | WQ_03_ms$Data.Source == 'Mass Emissions')& WQ_03_ms$Analyte %in% metals_ss),] 
WQ_ms_remined <- read.csv(file = 'LAR_metals&ss_database_ceden&me-remined.csv',
                      stringsAsFactors = FALSE) 
WQ_03_ms_3 <- rbind(WQ_03_ms_2, WQ_ms_remined) %>%
  mutate(ID = str_pad(row_number(), 6, pad = '0'))

WQ_04 <- select(WQ_03_ms_3, 'ID', 1:(ncol(WQ_03_ms_3)-1)) %>%
  filter(Data.Source != 'SMARTS')                     # SMARTs data is cleaned on its own later, so removed at this step


# Remove any observation with station code of "LABQA" -- has no significance to this data analysis
WQ_04 <- WQ_04[-c(grep('LABQA', WQ_04$StationCode)),]





# Step 05: Add missing coordinates and station names
# Code adapted from LookUpCode1.R by Victoria Hennon
# Edit file path to for lookup as appropriate
WQ_05 <- WQ_04
lookup <- read_excel("C:/Users/altinoco99/Dropbox/LA-River Flows-SCCWRP/04-data/water-quality/lat_long_lu_code/master_station_lu.xlsx")
add <- c("TargetLatitude", "TargetLongitude", "StationName")
WQ_05[(is.na(WQ_04$TargetLatitude) & is.na(WQ_04$TargetLongitude)), add] <- sapply(add, function(x) lookup[[x]][match(WQ_04$StationCode, lookup$StationCode)])[(is.na(WQ_04$TargetLatitude) & is.na(WQ_04$TargetLongitude)),]


# Remove observations with 0s or -88s as values for lat and longs
WQ_05 <- filter(WQ_05, (is.na(TargetLatitude) & is.na(TargetLongitude)) | (TargetLatitude != '0.0000000000' & TargetLongitude != '0.0000000000' & TargetLatitude != '-88' & TargetLongitude != '-88'))

# Convert lat and long to have class "numeric"
WQ_05 <- mutate(WQ_05, TargetLatitude = as.numeric(TargetLatitude), TargetLongitude = as.numeric(TargetLongitude))

# Convert positive 118.### in longitudes to be negative (longitudes in LA are roughly -118, input error in raw data)
WQ_05$TargetLongitude[WQ_05$TargetLongitude > 0 & !is.na(WQ_05$TargetLongitude)] <- WQ_05$TargetLongitude[WQ_05$TargetLongitude > 0 & !is.na(WQ_05$TargetLongitude)]*(-1)


# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_05, "LAR_WQ_database_02_coords.csv", row.names = FALSE)





# Step 06: Clean MatrixName column
# Some lines commented out as they applied to SMARTS data only, which is currently removed to be cleaned later
# Remove observations with unwanted matrix names
# Combine similar names and assigne appropriate names to those missing matrixnames
WQ_06 <- WQ_05[-c(grep('sediment|blank|interstitial|ground|sea|brackish|lab|reference', WQ_05$MatrixName, ignore.case = TRUE)),]
# WQ_06$MatrixName[grep('sample', WQ_06$MatrixName, ignore.case = TRUE)] <- 'SampleWater'
WQ_06$SampleTypeName[grep('grab', WQ_06$MatrixName, ignore.case = TRUE)] <- 'Grab'
WQ_06$MatrixName[grep('water|grab', WQ_06$MatrixName, ignore.case = TRUE)] <- 'SurfaceWater'
# WQ_06$MatrixName[grep('industrial', WQ_06$ProjectName, ignore.case = TRUE)] <- 'Industrial'
# WQ_06$MatrixName[grep('Construction', WQ_06$ProjectName, ignore.case = FALSE)] <- 'Construction'
WQ_06$MatrixName[WQ_06$Data.Source == 'Mass Emissions'] <- 'SurfaceWater'
WQ_06$MatrixName[WQ_06$Data.Source == 'MS4' & is.na(WQ_06$MatrixName)] <- 'SurfaceWater'
WQ_06$MatrixName[WQ_06$Data.Source == 'Watershed Health'] <- 'SurfaceWater'

# matrixnames <- sort(unique(WQ_06$MatrixName))



# Step 07: Clean SampleDate column
# Convert to have class "Date"
WQ_07 <- mutate(WQ_06, SampleDate = mdy(SampleDate))



# Step 08: Clean CollectionTime column
# Remove extra colons entered in some times
WQ_08 <- mutate(WQ_07, CollectionTime = gsub("::", ":", CollectionTime))
# Set blanks to NA
WQ_08$CollectionTime[WQ_08$CollectionTime == ''] <- NA
# Create DateTime column combining SampleData and CollectionTime columns
# Observations with no collection time had time set to 0:00
WQ_08 <- unite(WQ_08, 'DateTime', SampleDate, CollectionTime, sep = ' ', remove = FALSE) %>%
  mutate(DateTime = gsub('NA', '0:00', DateTime)) %>%
  mutate(DateTime = ymd_hm(DateTime))

# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_08, "LAR_WQ_database_03_matrix-date-time.csv", row.names = FALSE)



# Step 09: Clean SampleTypeName column
# Remove rows with unwanted sample type names & combine remaining appropriately
WQ_09 <- WQ_08[-c(grep('fieldbl|field blank|field dup|travel|CNDC|lab|LCS|MS1|PDS|PDD|QAGrab|QREC', WQ_08$SampleTypeName, ignore.case = TRUE)),]
WQ_06$SampleTypeName[WQ_09$Data.Source == 'Mass Emissions'] <- 'Grab'
WQ_09$SampleTypeName[grep('N/A|Not recorded|Not Applicable', WQ_09$SampleTypeName, ignore.case = TRUE)] <- NA
WQ_09$SampleTypeName[WQ_09$SampleTypeName == ''] <- NA
WQ_09 <- mutate(WQ_09, SampleTypeName = gsub(' ', '', WQ_09$SampleTypeName))
WQ_09$SampleTypeName[grep('Water_Grab|WQ|Field', WQ_09$SampleTypeName)] <- 'Grab'
WQ_09 <- mutate(WQ_09, SampleTypeName = gsub('Grab', 'Grab', WQ_09$SampleTypeName, ignore.case = TRUE))
WQ_09$SampleTypeName[grep('integrated|autosamplerstorm', WQ_09$SampleTypeName, ignore.case = TRUE)] <- 'Composite'
# WQ_09$SampleTypeName[grep('unspiked', WQ_09$SampleTypeName, ignore.case = TRUE)] <- 'Unspiked Sample'               # Applies to SMARTS data only
WQ_09$SampleTypeName[grep('derived', WQ_09$SampleTypeName, ignore.case = TRUE)] <- 'DERIVED DAILY'

# sampletypes <- sort(unique(WQ_09$SampleTypeName))




#Step 10: Clean Analyte column
# WQ_10 <- WQ_09[-c(grep('Unionized|percent|chemical|phosphate', WQ_09$Analyte, ignore.case = TRUE)),] %>%   # Applies to SMARTS data only
#   mutate(Analyte = gsub(' ', '', Analyte))

# Remove spaces, combine analyte names appropriately
WQ_10 <- WQ_09 %>%                           
    mutate(Analyte = gsub(' ', '', Analyte))

ammonia_as_N <- c('AMMONIA-N','Ammonia,Total(asN)','AmmoniaasN','AmmoniaasN,Total', 'NH3-N', 'AmmoniaasN,NotRecorded')
WQ_10$Analyte[WQ_10$Analyte %in% ammonia_as_N] <- 'Ammonia (as N)'

ammonia_as_NH3 <- c('AmmoniaasNH3', 'AmmoniaasNH3,Total', 'Ammonia')
WQ_10$Analyte[WQ_10$Analyte %in% ammonia_as_NH3] <- 'Ammonia (as NH3)'

WQ_10$Analyte[grep('AmmoniaasN,Dissolved', WQ_10$Analyte)] <- 'Ammonia, Dissolved (as N)'

WQ_10$Analyte[grep('conduct', WQ_10$Analyte, ignore.case = TRUE)] <- 'Conductivity'

copper_total <- c('Copper', 'Copper(N/A)', 'Copper(Total)', 'Copper,Total', 'Cu', 'COPPER(TOTALRECOVERABLE)', 'Copper,TotalRecoverable')
WQ_10$Analyte[WQ_10$Analyte %in% copper_total] <- 'Copper, Total'

copper_dissolved <- c('Copper(Dissolved)', 'Copper(Filtered)', 'Copper,Dissolved', 'Cu-dissolved', 'DissolvedCopper')
WQ_10$Analyte[WQ_10$Analyte %in% copper_dissolved] <- 'Copper, Dissolved'

DOC <- c('DissolvedOrganicCarbon', 'DissolvedOrganicCarbon,Dissolved')
WQ_10$Analyte[WQ_10$Analyte %in% DOC] <- 'Dissolved Organic Carbon (DOC)'

dissolved_oxygen <- c('DissolvedOxygen', 'DISSOLVEDOXYGEN', 'DO', 'OXYGEN(DISSOLVED)', 'Oxygen,Dissolved', 'Oxygen,Dissolved,Total', 'DissolvedOxygen,saturation')
WQ_10$Analyte[WQ_10$Analyte %in% dissolved_oxygen] <- 'Dissolved Oxygen (DO)'

e_coli <- WQ_10$Analyte[grep('coli', WQ_10$Analyte, ignore.case = TRUE)]
WQ_10$Analyte[WQ_10$Analyte %in% e_coli] <- 'E. coli'

lead_total <- c('Lead', 'Lead(N/A)', 'Lead(Total)', 'Lead,Total', 'Pb', 'LEAD(TOTALRECOVERABLE)', 'Lead,TotalRecoverable')
WQ_10$Analyte[WQ_10$Analyte %in% lead_total] <- 'Lead, Total'

lead_dissolved <- c('DissolvedLead', 'Lead(Dissolved)', 'Lead(Filtered)', 'Lead,Dissolved', 'Pb-dissolved')
WQ_10$Analyte[WQ_10$Analyte %in% lead_dissolved] <- 'Lead, Dissolved'

nitrate_as_N <- c('Nitrate-N', 'NITRATE-N', 'Nitrate(asN)', 'Nitrate,Total(asN)', 'NitrateasN', 'NitrateasN,Total', 'NitratesasN', 'NO3-N', 'NitrateasN,NotRecorded')
WQ_10$Analyte[WQ_10$Analyte %in% nitrate_as_N] <- 'Nitrate (as N)'

nitrate_as_NO3 <- c('Nitrate', 'Nitrate(NO3)', 'Nitrate,Total(asNO3)', 'NitrateasNO3', 'NitrateasNO3,Total')
WQ_10$Analyte[WQ_10$Analyte %in% nitrate_as_NO3] <- 'Nitrate (as NO3)'

WQ_10$Analyte[grep('NitrateasN,Dissolved', WQ_10$Analyte)] <- 'Nitrate, Dissolved (as N)'

nitrite_as_N <- c('Nitrite-N', 'NITRITE-N', 'Nitrite(asN)', 'Nitrite,Total(asN)', 'NitriteasN', 'NitriteasN,Total', 'NO2-N', 'NitriteasN,NotRecorded')
WQ_10$Analyte[WQ_10$Analyte %in% nitrite_as_N] <- 'Nitrite (as N)'

nitrite_as_NO2 <- c('Nitrite', 'Nitrite(NO2)', 'Nitrite,Total(asNO2)', 'NitriteasNO2', 'NitriteasNO2,Total')
WQ_10$Analyte[WQ_10$Analyte %in% nitrite_as_NO2] <- 'Nitrite (as NO2)'

WQ_10$Analyte[grep('NitriteasN,Dissolved|NitriteasN,Filtered', WQ_10$Analyte)] <- 'Nitrite, Dissolved (as N)'

nitrate_nitrite <- c('Nitrate+NitriteasN', 'Nitrate+NitriteasN,Total', 'NitrateasN+NitriteasN', 'NitrateplusNitriteNitrogen', 'NitritePlusNitrate(asN)', 'NO2-N+NO3-N', 'Nitrate+NitriteasN,NotRecorded')
WQ_10$Analyte[WQ_10$Analyte %in% nitrate_nitrite] <- 'Nitrate + Nitrite (as N)'

WQ_10$Analyte[grep('Nitrate+NitriteasN,Dissolved', WQ_10$Analyte)] <- 'Nitrate + Nitrite, Dissolved (as N)'

nitrogen_total <- c('N-TOTAL', 'NITROGEN(TOTAL)', 'Nitrogen,Total', 'Nitrogen,Total(asN)', 'Nitrogen,Total,NotRecorded', 'Nitrogen,Total,Total', 'TotalNitrogen')
WQ_10$Analyte[WQ_10$Analyte %in% nitrogen_total] <- 'Total Nitrogen'

organic_nitrogen <- c('Nitrogen,Organic', 'Nitrogen,Organic,Total', 'Nitrogen,TotalOrganic(asN)', 'ORGANIC-N', 'OrganicNitrogen')
WQ_10$Analyte[WQ_10$Analyte %in% organic_nitrogen] <- 'Organic Nitrogen'

WQ_10$Analyte[grep('Nitrogen,Inorganic', WQ_10$Analyte)] <- 'Inorganic Nitrogen'

TKN <- WQ_10$Analyte[grep('tkn|dahl', WQ_10$Analyte, ignore.case = TRUE)]
WQ_10$Analyte[WQ_10$Analyte %in% TKN] <- 'Total Kjeldahl Nitrogen (TKN)'

pH <- c('pH', 'PH-LAB', 'pH,Total')
WQ_10$Analyte[WQ_10$Analyte %in% pH] <- 'pH'

phosphorus_total <- c('Phosphorus', 'Phosphorus-Total', 'Phosphorus-Total(asP)', 'Phosphorus-Total(P)', 'PHOSPHORUS(TOTAL)','Phosphorus,Total','Phosphorus,Total(asP)', 'Phosphorus,Total,Total','PhosphorusasP', 'PhosphorusasP,Total', 'Phosphorus,TotalOrganic(asP)', 'TotalPhosphorus')
WQ_10$Analyte[WQ_10$Analyte %in% phosphorus_total] <- 'Phosphorus, Total'

phosphorus_dissolved <- c('DissolvedPhosphorus', 'Phosphorus,Dissolved','Phosphorus,Dissolved,Dissolved', 'PhosphorusasP,Dissolved')
WQ_10$Analyte[WQ_10$Analyte %in% phosphorus_dissolved] <- 'Phosphorus, Dissolved'

salinity <- c('Salinity', 'Salinity,Total')
WQ_10$Analyte[WQ_10$Analyte %in% salinity] <- 'Salinity'

suspended_sediment <- c('SuspendedSediment', 'SuspendedSedimentConcentration', 'SuspendedSedimentConcentration(SSC)',	'SuspendedSedimentConcentration,Particulate')
WQ_10$Analyte[WQ_10$Analyte %in% suspended_sediment] <- 'Suspended Sediment Concentration'

TDS <- c('Solids,TotalDissolved(Total)', 'TDS', 'TotalDissolvedSolids', 'TOTALDISSOLVEDSOLIDS', 'TotalDissolvedSolids(Dissolved)', 'TotalDissolvedSolids(N/A)', 'TotalDissolvedSolids(TDS)', 'TotalDissolvedSolids(Total)', 'TotalDissolvedSolids,Dissolved', 'TotalDissolvedSolids,Total')
WQ_10$Analyte[WQ_10$Analyte %in% TDS] <- 'Total Dissolved Solids (TDS)'

TSS <- c('Solids,TotalSuspended(Total)', 'SS', 'SUSPENDEDSOLIDS', 'SuspendedSolids(Total)', 'SuspendedSolids,Total', 'TotalSolids,Total', 'TotalSuspendedSolids', 'TotalSuspendedSolids(N/A)', 'TotalSuspendedSolids(Total)', 'TotalSuspendedSolids(TSS)', 'TotalSuspendedSolids(TSS),NetValue', 'TotalSuspendedSolids,Particulate', 'TotalSuspendedSolids,Total', 'TSS')
WQ_10$Analyte[WQ_10$Analyte %in% TSS] <- 'Total Suspended Solids (TSS)'

VSS <- WQ_10$Analyte[grep('volatile|vss', WQ_10$Analyte, ignore.case = TRUE)]
WQ_10$Analyte[WQ_10$Analyte %in% VSS] <- 'Volatile Suspended Solids (VSS)'

TOC <- c('TOC', 'TotalOrganicCarbon', 'TOTALORGANICCARBON', 'TotalOrganicCarbon(TOC)', 'TotalOrganicCarbon,Total')
WQ_10$Analyte[WQ_10$Analyte %in% TOC] <- 'Total Organic Carbon (TOC)'

zinc_total <- c('Zinc', 'Zinc(N/A)', 'Zinc(Total)', 'Zinc,Total', 'Zn', 'ZINC(TOTALRECOVERABLE)', 'Zinc,TotalRecoverable')
WQ_10$Analyte[WQ_10$Analyte %in% zinc_total] <- 'Zinc, Total'

zinc_dissolved <- c('DissolvedZinc', 'Zinc(Dissolved)', 'Zinc(Filtered)', 'Zinc,Dissolved', 'Zn-dissolved')
WQ_10$Analyte[WQ_10$Analyte %in% zinc_dissolved] <- 'Zinc, Dissolved'


# analytes <- sort(unique(WQ_10$Analyte))




#Step 11: Clean Unit column
# Combine units appropriately
WQ_11 <- WQ_10
WQ_11$Unit[grep('none|N/A', WQ_11$Unit, ignore.case = TRUE)] <- NA
WQ_11$Unit[grep('MPN', WQ_11$Unit, ignore.case = TRUE)] <- 'MPN/100mL'
WQ_11$Unit[grep('mg', WQ_11$Unit, ignore.case = TRUE)] <- 'mg/L'
WQ_11$Unit[grep('%', WQ_11$Unit, ignore.case = TRUE)] <- '%'
WQ_11$Unit[grep('ug', WQ_11$Unit, ignore.case = TRUE)] <- 'ug/L'
WQ_11$Unit[grep('umhos', WQ_11$Unit, ignore.case = TRUE)] <- 'umhos/cm'
WQ_11$Unit[grep('mL/L', WQ_11$Unit, ignore.case = TRUE)] <- 'mg/L'
WQ_11$Unit[WQ_11$Analyte == 'pH'] <- 'pH'
WQ_11$Unit[grep('uS/cm', WQ_11$Unit, ignore.case = TRUE)] <- 'uS/cm'


# units <- sort(unique(WQ_11$Unit))

# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_11, "LAR_WQ_database_04_sampletype-analyte-unit.csv", row.names = FALSE)




# Step 12: Clean Result column

# Remove rows with result 'AE" -- don't know what this means
# Create upper limit and lower limit columns for interval-endpoint format storage of data
WQ_12 <- WQ_11[-c(grep('AE', WQ_11$Result, ignore.case = TRUE)),] %>%
  mutate(Result = gsub(',', '', Result), LL = NA, UL = NA)
WQ_12 <- select(WQ_12, 1:18, LL, UL, 19:21)

# Set appropriate strings to NA
WQ_12$Result[(WQ_12$Result == '')|(WQ_12$Result == 'N/A')|(WQ_12$Result == 0)] <- NA
WQ_12$Result[grep('-', WQ_12$Result)] <- NA

# Set ResultQualCode to NR for appropriate results
WQ_12$ResultQualCode[grep('no |ns|CM|Not Analyzed', WQ_12$Result, ignore.case = TRUE)] <- 'NR'
WQ_12$Result[grep('no |ns|CM|Not Analyzed', WQ_12$Result, ignore.case = TRUE)] <- NA

# Set ResultQualCode to ND for results of ND
WQ_12$ResultQualCode[grep('ND', WQ_12$Result, ignore.case = TRUE)] <- 'ND'
WQ_12$Result[grep('ND', WQ_12$Result, ignore.case = TRUE)] <- NA

# Set ResultQualCode to DNQ for results of DNQ
WQ_12$ResultQualCode[grep('dnq', WQ_12$Result, ignore.case = TRUE)] <- 'DNQ'
WQ_12$Result[grep('DNQ', WQ_12$Result)] <- NA
WQ_12 <- mutate(WQ_12, Result = gsub("dnq|[(]|[)]|est", '', Result))

# Set ResultsQualCode to > for results of >## and set LL and UL appropriately
greater_than <- grep('>', WQ_12$Result)
WQ_12$ResultQualCode[greater_than] <- '>'
WQ_12 <- mutate(WQ_12, Result = gsub('>', '', Result))
WQ_12$LL[greater_than] <- WQ_12$Result[greater_than]
WQ_12$UL[greater_than] <- Inf
WQ_12$Result[greater_than] <- NA

# Set ResultQualCode to < for results of <
less_than <- grep('<', WQ_12$Result)
WQ_12$ResultQualCode[less_than] <- '<'
WQ_12 <- mutate(WQ_12, Result = gsub('<', '', Result))
WQ_12$MDL[less_than] <- WQ_12$Result[less_than]
WQ_12$Result[less_than] <- NA

# Coerce Results column to numeric
WQ_12 <- mutate(WQ_12, Result = as.numeric(Result))



# Step 13: Clean ResultQualCode column
# Remove spaces
WQ_13 <- mutate(WQ_12, ResultQualCode = gsub(' ', '', WQ_12$ResultQualCode))
# Combine values appropriately
WQ_13$ResultQualCode[grep('[0-9]|None', WQ_13$ResultQualCode)] <- NA
WQ_13$ResultQualCode[grep('J', WQ_13$ResultQualCode)] <- 'DNQ'
WQ_13$ResultQualCode[grep('P', WQ_13$ResultQualCode)] <- '='


# resqual <- sort(unique(WQ_13$ResultQualCode))



# Step 14: Clean MDL and RL columns
# Set appropriate strings to NA
WQ_14 <- WQ_13
WQ_14$MDL[grep('-|N/A', WQ_14$MDL)] <- NA
WQ_14$MDL[WQ_14$MDL == 0] <- NA
WQ_14$RL[grep('-|N/A', WQ_14$RL)] <- NA
WQ_14$RL[WQ_14$RL == 0] <- NA

# Coerce to be numeric
WQ_14 <- mutate(WQ_14, MDL = as.numeric(MDL), RL = as.numeric(RL))


# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_14, "LAR_WQ_database_05_result.csv", row.names = FALSE)




# Further cleaning of ResultQualCode
# Combine result qual codes as appropriate, set all pH to be =
WQ_15 <- WQ_14
WQ_15$ResultQualCode[is.na(WQ_15$ResultQualCode)] <- '='
WQ_15$ResultQualCode[WQ_15$ResultQualCode == '<='] <- '='
WQ_15$ResultQualCode[WQ_15$ResultQualCode == '>' & WQ_15$Analyte == 'pH'] <- '='
WQ_15$ResultQualCode[WQ_15$ResultQualCode == '>='] <- '>'
WQ_15$ResultQualCode[WQ_15$ResultQualCode == 'ND' & is.na(WQ_15$MDL) & !is.na(WQ_15$Result) & WQ_15$Result > 1000] <- '>'

# Fill out LL and UL columns for > values
greater_than_qual <- (WQ_15$ResultQualCode == '>') & (is.na(WQ_15$LL)) & 
  ((WQ_15$Data.Source == 'CEDEN') | (WQ_15$Data.Source == 'MS4') | (WQ_15$Data.Source == 'Watershed Health'))
WQ_15$LL[greater_than_qual] <- WQ_12$Result[greater_than_qual]
WQ_15$UL[greater_than_qual] <- Inf
WQ_15$Result[greater_than_qual] <- NA

# Set ND and DNQs with only and MDL or only and RL to be <
# Change result to NA
less_than_new <- (WQ_15$ResultQualCode == 'ND' | WQ_15$ResultQualCode == 'DNQ') & ((is.na(WQ_15$MDL) & !is.na(WQ_15$RL)) | (is.na(WQ_15$RL) & !is.na(WQ_15$MDL)))
WQ_15$ResultQualCode[less_than_new] <- '<'
WQ_15$Result[less_than_new] <- NA


# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_15, "LAR_WQ_database_06_result_no-smarts.csv", row.names = FALSE)


# Remove rows with results that could not be interpreted
rows_to_remove <- (WQ_15$ResultQualCode == 'DNQ' & is.na(WQ_15$MDL) & WQ_15$Data.Source == 'Watershed Health') |
  (WQ_15$ResultQualCode == 'ND' & WQ_15$Result == 18 & !is.na(WQ_15$Result)) |
  ((WQ_15$ResultQualCode != 'NR') & is.na(WQ_15$Result) & is.na(WQ_15$MDL) & is.na(WQ_15$RL) & is.na(WQ_15$LL))
WQ_16 <- WQ_15[!rows_to_remove,]



# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_16, "LAR_WQ_database_07_removed_no-smarts.csv", row.names = FALSE)



# Continue cleaning ResultQualCode column
WQ_17 <- WQ_16

# Set values that are greater than the RL and MDL to be =
WQ_17$ResultQualCode[!is.na(WQ_17$Result) & !is.na(WQ_17$RL) & (WQ_17$Result > WQ_17$RL)] <- "="

# Set values that are less than the MDL to be ND
non_detect <- !is.na(WQ_17$MDL) & !is.na(WQ_17$Result) & (WQ_17$Result < WQ_17$MDL)
WQ_17$ResultQualCode[non_detect] <- 'ND'
WQ_17$Result[non_detect] <- NA

# Set values that are between the MDL and RL to be DNQ
dnq <- !is.na(WQ_17$MDL) & !is.na(WQ_17$RL) & !is.na(WQ_17$Result) & (WQ_17$Result < WQ_17$RL) & (WQ_17$Result > WQ_17$MDL)
WQ_17$ResultQualCode[dnq] <- 'DNQ'
WQ_17$Result[dnq] <- NA


# Set result for ND, DNQ and < to be NA
WQ_17$Result[WQ_17$ResultQualCode == 'DNQ' & !is.na(WQ_17$Result) & !is.na(WQ_17$MDL) & (WQ_17$Result == WQ_17$MDL | WQ_17$Result == WQ_17$RL)] <- NA

WQ_17$Result[(WQ_17$ResultQualCode == 'ND' | WQ_17$ResultQualCode == '<') & !is.na(WQ_17$Result) & !is.na(WQ_17$MDL) & WQ_17$Result == WQ_17$MDL] <- NA

# Change all < to be ND
WQ_17$ResultQualCode[WQ_17$ResultQualCode == '<'] <- 'ND'



# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(WQ_17, "LAR_WQ_database_08_resqual_no-smarts.csv", row.names = FALSE)


# Set LL and UL for NDs that don't have an MDL, so are less than the RL 
WQ_18 <- WQ_17
less_than_RL <- WQ_18$ResultQualCode == 'ND' & !is.na(WQ_18$RL) & !is.na(WQ_18$Result)
WQ_18$UL[less_than_RL] <- WQ_17$Result[less_than_RL]
WQ_18$LL[less_than_RL] <- 0
WQ_18$Result[less_than_RL] <- NA

# Set RL for ND and DNQ values missing RL and MDL values to result value
replace_RL <- (WQ_18$ResultQualCode == 'ND' | WQ_18$ResultQualCode == 'DNQ') & is.na(WQ_18$RL) & !is.na(WQ_18$Result)
# WQ_18$UL[replace_RL] <- WQ_18$Result[replace_RL]
# WQ_18$LL[replace_RL] <- 0
WQ_18$RL[replace_RL] <- WQ_18$Result[replace_RL]
WQ_18$Result[replace_RL] <- NA


# Set NA results with qual code of = to have qual code of NR
WQ_18$ResultQualCode[WQ_18$ResultQualCode == '=' & is.na(WQ_18$Result)] <- 'NR'



# Remove any duplicate rows based on all columns EXCEPT ID
WQ_19 <- distinct(WQ_18, Data.Source, StationCode, DateTime, SampleDate, ProjectName,
                  CollectionTime, SampleTypeName, CollectionDepth, UnitCollectionDepth,
                  MatrixName, MethodName, Analyte, Unit, Result, ResultQualCode, MDL,
                  RL, TargetLatitude, TargetLongitude, StationName, .keep_all = TRUE)

WQ_20 <- WQ_19

# Assign LL and UL values
dnq_with_mdl <- WQ_20$ResultQualCode == 'DNQ' & !is.na(WQ_20$MDL)
WQ_20$UL[dnq_with_mdl] <- WQ_20$RL[dnq_with_mdl]
WQ_20$LL[dnq_with_mdl] <- WQ_20$MDL[dnq_with_mdl]

dnq_wo_mdl <- WQ_20$ResultQualCode == 'DNQ' & is.na(WQ_20$MDL)
WQ_20$UL[dnq_wo_mdl] <- WQ_20$RL[dnq_wo_mdl]
WQ_20$LL[dnq_wo_mdl] <- 0

nd_with_mdl <- WQ_20$ResultQualCode == 'ND' & !is.na(WQ_20$MDL) & is.na(WQ_20$UL)
WQ_20$UL[nd_with_mdl] <- WQ_20$MDL[nd_with_mdl]
WQ_20$LL[nd_with_mdl] <- 0

nd_wo_mdl <- WQ_20$ResultQualCode == 'ND' & is.na(WQ_20$MDL) & is.na(WQ_20$UL)
WQ_20$UL[nd_wo_mdl] <- WQ_20$RL[nd_wo_mdl]
WQ_20$LL[nd_wo_mdl] <- 0

equal <- WQ_20$ResultQualCode == '='
WQ_20$UL[equal] <- WQ_20$Result[equal]
WQ_20$LL[equal] <- WQ_20$Result[equal]

# Remove rows still missing lat/long coordinates
WQ_21 <- filter(WQ_20, !is.na(WQ_20$TargetLatitude) & !is.na(WQ_20$TargetLongitude))

# Clean CollectionDepth columne
WQ_21$CollectionDepth[WQ_21$CollectionDepth == '-88'] <- NA
WQ_21 <- mutate(WQ_21, CollectionDepth = as.numeric(CollectionDepth))

#Set missing station names to NA
WQ_21$StationName[WQ_21$StationName == ''] <- NA


# write.csv(WQ_21, "LAR_WQ_database_09_UL-LL_no-smarts.csv", row.names = FALSE)

# Export final .csv file with added date
write.csv(WQ_21, "LAR_WQ_database_11_missing-data-added_no-smarts.csv", row.names = FALSE)









# Cleaning SMARTS data only
# The number in smarts_## roughly corresponds to the steps completed for the other WQ data cleaning
# ID columns already added from original import
# Filter for only SMARTS data
smarts_04 <- select(WQ_03, 'ID', 1:(ncol(WQ_03)-1)) %>%
  filter(Data.Source == 'SMARTS')                     



# Lat and Long are already numeric, skipping step 05
# MatrixName already combined, skipping step 06



# Step 07: Clean SampleData column
smarts_07 <- mutate(smarts_04, SampleDate = mdy(SampleDate))



# Step 08: Clean CollectionTime column
smarts_08 <- mutate(smarts_07, CollectionTime = gsub("::", ":", CollectionTime))
smarts_08$CollectionTime[smarts_08$CollectionTime == ''] <- NA
smarts_08 <- unite(smarts_08, 'DateTime', SampleDate, CollectionTime, sep = ' ', remove = FALSE) %>%
  mutate(DateTime = gsub('NA', '0:00', DateTime)) %>%
  mutate(DateTime = ymd_hm(DateTime))



# SampleTypeName is all NA, skipping step 09



# Step 10: Clean Analyte column
smarts_10 <- smarts_08[-c(grep('Unionized|percent|chemical|phosphate', smarts_08$Analyte, ignore.case = TRUE)),] %>%
    mutate(Analyte = gsub(' ', '', Analyte))

smarts_10$Analyte[smarts_10$Analyte %in% ammonia_as_N] <- 'Ammonia (as N)'

smarts_10$Analyte[smarts_10$Analyte %in% copper_total] <- 'Copper, Total'

smarts_10$Analyte[smarts_10$Analyte %in% copper_dissolved] <- 'Copper, Dissolved'

smarts_10$Analyte[smarts_10$Analyte %in% DOC] <- 'Dissolved Organic Carbon (DOC)'

smarts_10$Analyte[smarts_10$Analyte %in% dissolved_oxygen] <- 'Dissolved Oxygen (DO)'

smarts_10$Analyte[smarts_10$Analyte %in% e_coli] <- 'E. coli'

smarts_10$Analyte[smarts_10$Analyte %in% lead_total] <- 'Lead, Total'

smarts_10$Analyte[smarts_10$Analyte %in% lead_dissolved] <- 'Lead, Dissolved'

smarts_10$Analyte[smarts_10$Analyte %in% nitrate_as_N] <- 'Nitrate (as N)'

smarts_10$Analyte[smarts_10$Analyte %in% nitrate_as_NO3] <- 'Nitrate (as NO3)'

smarts_10$Analyte[smarts_10$Analyte %in% nitrite_as_N] <- 'Nitrite (as N)'

smarts_10$Analyte[smarts_10$Analyte %in% nitrite_as_NO2] <- 'Nitrite (as NO2)'

smarts_10$Analyte[smarts_10$Analyte %in% nitrate_nitrite] <- 'Nitrate + Nitrite (as N)'

smarts_10$Analyte[smarts_10$Analyte %in% nitrogen_total] <- 'Total Nitrogen'

smarts_10$Analyte[smarts_10$Analyte %in% TKN] <- 'Total Kjeldahl Nitrogen (TKN)'

smarts_10$Analyte[smarts_10$Analyte %in% pH] <- 'pH'

smarts_10$Analyte[smarts_10$Analyte %in% phosphorus_total] <- 'Phosphorus, Total'

smarts_10$Analyte[smarts_10$Analyte %in% suspended_sediment] <- 'Suspended Sediment Concentration'

smarts_10$Analyte[smarts_10$Analyte %in% TDS] <- 'Total Dissolved Solids (TDS)'

smarts_10$Analyte[smarts_10$Analyte %in% TSS] <- 'Total Suspended Solids (TSS)'

smarts_10$Analyte[smarts_10$Analyte %in% VSS] <- 'Volatile Suspended Solids (VSS)'

smarts_10$Analyte[smarts_10$Analyte %in% TOC] <- 'Total Organic Carbon (TOC)'

smarts_10$Analyte[smarts_10$Analyte %in% zinc_total] <- 'Zinc, Total'

smarts_10$Analyte[smarts_10$Analyte %in% zinc_dissolved] <- 'Zinc, Dissolved'



# Step 11: Clean Unit column
smarts_11 <- smarts_10
smarts_11$Unit[grep('none|N/A', smarts_11$Unit, ignore.case = TRUE)] <- NA
smarts_11$Unit[grep('MPN', smarts_11$Unit, ignore.case = TRUE)] <- 'MPN/100mL'
smarts_11$Unit[grep('mg', smarts_11$Unit, ignore.case = TRUE)] <- 'mg/L'
smarts_11$Unit[grep('%', smarts_11$Unit, ignore.case = TRUE)] <- '%'
smarts_11$Unit[grep('ug', smarts_11$Unit, ignore.case = TRUE)] <- 'ug/L'
smarts_11$Unit[grep('umhos', smarts_11$Unit, ignore.case = TRUE)] <- 'umhos/cm'
smarts_11$Unit[grep('mL/L', smarts_11$Unit, ignore.case = TRUE)] <- 'mg/L'
smarts_11$Unit[grep('pH', smarts_11$Analyte, ignore.case = TRUE)] <- 'pH'
smarts_11$Unit[grep('uS/cm', smarts_11$Unit, ignore.case = TRUE)] <- 'uS/cm'



# Step 12: Clean Result column (and clean ResultQualCode, MDL, and RL, and create UL and LL columns)
smarts_12 <- smarts_11 %>%
  mutate(LL = NA, UL = NA, Result = as.numeric(Result), MDL = as.numeric(MDL), RL = as.numeric(RL))
smarts_12 <- select(smarts_12, 1:18, LL, UL, 19:21)

smarts_12$Result[(smarts_12$Result == '')|(smarts_12$Result == 0)] <- NA
smarts_12$MDL[(smarts_12$MDL == -88)|(smarts_12$MDL == 0)] <- NA
smarts_12$RL[smarts_12$RL == 0] <- NA



# write.csv(smarts_12, "LAR_SMARTS_database_01.csv", row.names = FALSE)



smarts_13 <- smarts_12

# Clean results for pH
ph_nr <- smarts_13$Analyte == 'pH' & (smarts_13$ResultQualCode == 'ND' | smarts_13$ResultQualCode == '<' | smarts_13$ResultQualCode == 'DNQ') & is.na(smarts_13$Result)
smarts_13$ResultQualCode[ph_nr] <- 'NR'

ph_equal <- smarts_13$Analyte == 'pH' & (smarts_13$ResultQualCode == 'ND' | smarts_13$ResultQualCode == '<' | smarts_13$ResultQualCode == 'DNQ' | smarts_13$ResultQualCode == '>') & !is.na(smarts_13$Result)
smarts_13$ResultQualCode[ph_equal] <- '='

ph_remove <- smarts_13$Analyte == 'pH' & !is.na(smarts_13$Result) & (smarts_13$Result > 14)
smarts_14 <- smarts_13[!ph_remove,]

# Change > not on E. coli observations to =
smarts_14$ResultQualCode[smarts_14$ResultQualCode == '>' & smarts_14$Analyte != 'E. coli'] <- '='

# Set LL and UL appropriately for > values
e_coli_greater_than <- smarts_14$ResultQualCode == '>' & smarts_14$Analyte == 'E. coli'
smarts_14$UL[e_coli_greater_than] <- Inf
smarts_14$LL[e_coli_greater_than] <- smarts_14$Result[e_coli_greater_than]
smarts_14$Result[e_coli_greater_than] <- NA

# Set values less than MDL to be ND
non_detect_mdl <- !is.na(smarts_14$Result) & !is.na(smarts_14$MDL) & smarts_14$Result < smarts_14$MDL & smarts_14$Analyte != 'pH'
smarts_14$ResultQualCode[non_detect_mdl] <- 'ND'
smarts_14$Result[non_detect_mdl] <- NA

# Set values between MDL and RL to be DNQ
dnq_smarts <- !is.na(smarts_14$Result) & !is.na(smarts_14$MDL) & !is.na(smarts_14$RL) & smarts_14$Result > smarts_14$MDL & smarts_14$Result < smarts_14$RL & smarts_14$Analyte != 'pH'
smarts_14$ResultQualCode[dnq_smarts] <- 'DNQ'
smarts_14$Result[dnq_smarts] <- NA

# Set values greater the MDL and RL to be =
equal_smarts <- (!is.na(smarts_14$Result) & !is.na(smarts_14$RL) & smarts_14$Result > smarts_14$RL) | (!is.na(smarts_14$Result) & is.na(smarts_14$RL) & !is.na(smarts_14$MDL) & smarts_14$Result > smarts_14$MDL)
smarts_14$ResultQualCode[equal_smarts] <- '='

# Set all < to be ND
smarts_14$ResultQualCode[smarts_14$ResultQualCode == '<'] <- 'ND'


# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(smarts_14, "LAR_SMARTS_database_02.csv", row.names = FALSE)


# Remove unusable data
na_remove <- smarts_14$ResultQualCode != 'NR' & is.na(smarts_14$Result) & is.na(smarts_14$MDL) & is.na(smarts_14$RL)
smarts_15 <- smarts_14[!na_remove,]

# Set results of NA with qual code of = to be NR
smarts_15$ResultQualCode[smarts_15$ResultQualCode == '=' & is.na(smarts_15$Result)] <- 'NR'

# Set DNQs missing RL to be ND
smarts_15$ResultQualCode[smarts_15$ResultQualCode == 'DNQ' & is.na(smarts_15$RL) & !is.na(smarts_15$MDL)] <- 'ND'

# Set results for NDs and DNQs to be NA
smarts_15$Result[smarts_15$ResultQualCode == 'DNQ'] <- NA

smarts_15$Result[smarts_15$ResultQualCode == 'ND' & !is.na(smarts_15$Result) & !is.na(smarts_15$MDL) & smarts_15$Result == smarts_15$MDL] <- NA

# Set LL and UL for observations where result equals RL
nd_res_RL <- smarts_15$ResultQualCode == 'ND' & !is.na(smarts_15$Result) & !is.na(smarts_15$RL) & smarts_15$Result == smarts_15$RL
smarts_15$UL[nd_res_RL] <- smarts_15$Result[nd_res_RL]
smarts_15$LL[nd_res_RL] <- 0
smarts_15$Result[nd_res_RL] <- NA



# Export .csv file for viewing in Excel (uncomment to use)
# write.csv(smarts_15, "LAR_SMARTS_database_03.csv", row.names = FALSE)



smarts_16 <- smarts_15

# Set LL and UL for NDs with and RL but not MDL
nd_RL <- smarts_16$ResultQualCode == 'ND' & is.na(smarts_16$RL) & !is.na(smarts_16$Result)
smarts_16$RL[nd_RL] <- smarts_16$Result[nd_RL]
smarts_16$UL[nd_RL] <- smarts_16$Result[nd_RL]
smarts_16$LL[nd_RL] <- 0
smarts_16$Result[nd_RL] <- NA

# Assign MDLs for NDs with results but no MDL
nd_fix <- smarts_16$ResultQualCode == 'ND' & !is.na(smarts_16$RL) & !is.na(smarts_16$Result)
smarts_16$MDL[nd_fix] <- smarts_16$Result[nd_fix]
smarts_16$Result[nd_fix] <- NA

# Assign UL and LL for DNQ values
dnq_limits <- smarts_16$ResultQualCode == 'DNQ'
smarts_16$UL[dnq_limits] <- smarts_16$RL[dnq_limits]
smarts_16$LL[dnq_limits] <- smarts_16$MDL[dnq_limits]

# Assign UL and LL for = values
equal_limits <- smarts_16$ResultQualCode == '='
smarts_16$UL[equal_limits] <- smarts_16$Result[equal_limits]
smarts_16$LL[equal_limits] <- smarts_16$Result[equal_limits]

# Assigne UL and LL for ND values
nd_limits <- smarts_16$ResultQualCode == 'ND' & is.na(smarts_16$LL)
smarts_16$UL[nd_limits] <- smarts_16$MDL[nd_limits]
smarts_16$LL[nd_limits] <- 0

# Remove duplicate rows
smarts_17 <- distinct(smarts_16, Data.Source, StationCode, DateTime, SampleDate, ProjectName,
                  CollectionTime, SampleTypeName, CollectionDepth, UnitCollectionDepth,
                  MatrixName, MethodName, Analyte, Unit, Result, ResultQualCode, MDL,
                  RL, TargetLatitude, TargetLongitude, StationName, .keep_all = TRUE)

# Remove rows missing lat/longs coords
smarts_18 <- filter(smarts_17, !is.na(smarts_17$TargetLatitude) | !is.na(smarts_17$TargetLongitude))



# Export final .csv file
# write.csv(smarts_18, "LAR_SMARTS_database_04.csv", row.names = FALSE)


