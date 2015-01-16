## Steps:
##
## (1) First get the area harvested/sown data using the R API (GetData)
##
## (2) Then get the seed rate data after loading them in to the data
##     base. (GetTableData).
##
## NOTE (Michael): The codes need to be converted to CPC from FCL.
##
##
## (3) Multiply the seed rate with the area harvested/sown in the
##     following year to get the seed used in the current year.
##
## (4) Save the data back.

library(data.table)
library(faosws)
library(faoswsFlag)
library(faoswsUtil)


## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"

## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "ad7f16e3-d447-48ec-9d62-089f63bbc137"
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}

## Function for obtaining the area harvested/sown data
getAreaData = function(dataContext){
    ## Setups

    ## setting the prefix, also should be accessed by the API
    prefixTuples =
        data.table(
            valuePrefix = "Value_measuredElement_",
            flagObsPrefix = "flagObservationStatus_measuredElement_",
            flagMethodPrefix = "flagMethod_measuredElement_"
            )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )

    ## Check the code for area
    slot(slot(dataContext, "dimensions")$measuredElement, "keys") =
        c("5212", "5312")
    
    ## Query the data
    query = GetData(
        key = dataContext,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    
    lapply(c("5212", "5312"),
           FUN = function(x){
               remove0M(data = query,
                        value = paste0(prefixTuples$valuePrefix, x),
                        flag = paste0(prefixTuples$flagObsPrefix, x))
           })
    query[, timePointYears := as.numeric(timePointYears)]
    setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    list(query = query,
         prefixTuples = prefixTuples)
}

## Get the area harvested/sown data
areaData = getAreaData(swsContext.datasets[[1]])

## Function to compute the area sown for calculation of seed, this
## function should be re-written.
imputeAreaSown = function(valueAreaSown, valueAreaHarvested, flagObsAreaSown,
    flagObsAreaHarvested, imputedFlag = "i"){
    if(all(is.na(valueAreaSown))){
        valueAreaSown = valueAreaHarvested
        flagObsAreaSown = flagObsAreaHarvested
    } else {
        ratio = mean(valueAreaSown/valueAreaHarvested, na.rm = TRUE)
        replaceIndex = is.na(valueAreaSown) & !is.na(valueAreaHarvested)
        valueAreaSown[replaceIndex] =
            valueAreaHarvested[replaceIndex] * ratio
        flagObsAreaSown[replaceIndex] = flagObsAreaHarvested[replaceIndex]
    }
    list(valueAreaSown, flagObsAreaSown, imputedFlag)
}

areaData$query[, `:=`(c("Value_measuredElement_5212",
                        "flagObservation_measuredElement_5212",
                        "flagMethod_measuredElement_5212"),
                      imputeAreaSown(Value_measuredElement_5212,
                                     Value_measuredElement_5312,
                                     flagObservationStatus_measuredElement_5212,
                                     flagObservationStatus_measuredElement_5312))]


## Merge this with the item name info then convert it to cpc
##
## This is country specific
seed.dt = data.table(read.csv(file = "specific_seed_rate.csv"))
table(seed.dt$faostatFlag_seedRate)


## NOTE (Michael): We assume all other data collection method are official
seed.dt[faostatFlag_seedRate != "E", faostatFlag_seedRate := ""]


## Modify the data to conform to the new data base structure.
countrySpecificRate.dt =
    seed.dt[, list(geographicAreaFS, measuredItemFS, Value_seedRate,
                   faostatFlag_seedRate)]
## setnames(countrySpecificRate.dt, old = colnames(countrySpecificRate.dt),
##          new = c("geographicAreaM49", "measuredItemCPC", "seedRate", "seedFlag"))
countrySpecificRate.dt[, measuredItemCPC := as.character(measuredItemCPC)]
countrySpecificRate.dt[, geographicAreaM49 := as.character(geographicAreaM49)]


## This is a temporary hack to change the item and area classification
countrySpecificRate.dt[measuredItemCPC == "15", measuredItemCPC := "0111"]
countrySpecificRate.dt[geographicAreaM49 == "231", geographicAreaM49 := "840"]
setkeyv(countrySpecificRate.dt, c("geographicAreaM49", "measuredItemCPC"))

countrySpecificRate.dt =
    GetTableData(schemaName = "ess", tableName = "specific_seed_rate")
setnames(countrySpecificRate.dt,
         old = c("area", "item", "value", "flag"),
         new = c("geographicAreaM49", "measuredItemCPC", "Value_seedRate",
             "flagObservation_seedRate"))
setkeyv(countrySpecificRate.dt, c("geographicAreaM49", "measuredItemCPC"))



## Fill in the country Specific rates
areaData$query[countrySpecificRate.dt, `:=`(c("seedRate", "seedFlag"),
                                            list(i.Value_seedRate,
                                                 i.flagObservation_seedRate)),
               allow.cartesian = TRUE]


## Modify the general rates
seedGeneral.dt = data.table(read.csv(file = "general_seed_rate.csv"))
setnames(seedGeneral.dt, old = colnames(seedGeneral.dt),
         new = c("measuredItemNameCPC", "measuredItemCPC", "seedRate"))
seedGeneral.dt[, measuredItemCPC := as.character(measuredItemCPC)]
seedGeneral.dt[measuredItemCPC == "15", measuredItemCPC := "0111"]
setkeyv(seedGeneral.dt, "measuredItemCPC")
setcolorder(seedGeneral.dt,
            neworder = c("measuredItemCPC", "measuredItemNameCPC", "seedRate"))
seedGeneral.dt[, seedFlag := ""]

## Add in the flag
seedGeneral.dt = GetTableData(schemaName = "ess", tableName = "default_seed_rate")
setnames(seedGeneral.dt,
         old = c("item", "value"),
         new = c("measuredItemCPC", "Value_seedRate"))
setkeyv(seedGeneral.dt, "measuredItemCPC")



## fill in the general rates
okey = key(areaData$query)
setkeyv(areaData$query, key(seedGeneral.dt))
areaData$query[is.na(seedRate), ][seedGeneral.dt,
                                  `:=`(c("seedRate"),
                                       list(i.Value_seedRate)),
                                  allow.cartesian = TRUE]
setkeyv(areaData$query, okey)

## Function to impute the seed.
imputeSeed = function(areaSown, seedRate, missingFlag = "M", imputedFlag = "i"){
    seed = c(c(areaSown, NA) * c(NA, seedRate)/1000)[-1]
    seedFlag = rep(missingFlag, length(areaSown))
    seedFlag[!is.na(seedFlag)] = imputedFlag
    list(seed, seedFlag)
}
    
## Compute the seed and it's flag
areaData$query[, `:=`(c("Value_measuredElement_5525",
                        "flagMethod_measuredElement_5525"),
                      imputeSeed(Value_measuredElement_5212, seedRate))]

## This is temporary, since we don't have table for seed.
areaData$query[, flagObservationStatus_measuredElement_5525 :=
                   aggregateObservationFlag(flagObservationStatus_measuredElement_5212, seedFlag)]
areaData$query[, seedRate := NULL]
areaData$query[, seedFlag := NULL]

## Save the data back
SaveData(domain = "agriculture", dataset = "agriculture", data = areaData$query,
         normalized = FALSE)



## NOTE (Michael): Need to check whether all the seed rate data has
##                 been included.
##
## NOTE (Michael): Need to check with Adam where the data came from?
##
## NOTE (Michael): Need to convert the seed.rate in the AllSeed
##                 data.frame to numeric.
##
## NOTE (Michael): It looks like the seed data is the country and year
##                 specific rates, while the AllSeed is the default
##                 rates for each commodity.
##
## NOTE (Michael): The multiplication should be done to the next year,
##                 not the current year.
##
## NOTE (Michael): What are the nutrition value for?
##
## NOTE (Michael): All the codes are still in FCL
##
## NOTE (Michael): Remove the hard coded china, and also 351 is an aggregate.
##
## NOTE (Michael): Write the seed module at the item level.
