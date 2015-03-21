suppressMessages({
    library(lattice)
    library(faosws)
    library(faoswsUtil)
    library(lme4)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(igraph)
})


## Year should be a paramameter selected.
selectedYear = as.character(1992:2015)

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "007e9eea-5766-41c8-9495-8ad7be4124cc"
        )
    R_SWS_SHARE_PATH = getwd()
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
    updateModel = as.logical(swsContext.computationParams$updateModel)
}



## Function to obtain all CPC item 
getAllItemCPC = function(){
    itemEdgeList =
        adjacent2edge(
            GetCodeTree(domain = "agriculture",
                        dataset = "agriculture",
                        dimension = itemVar)
        )
    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}

getAllArea = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "geographicAreaM49")[type == "country", code]
}


getOfficialSeedData = function(){
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredArea),
            Dimension(name = elementVar,
                      keys = "5525"),
            Dimension(name = itemVar,
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    seedPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )

    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    seedQuery[flagObservationStatus_measuredElement_5525 == "", ]

}


getAreaData = function(){
    ## Setups    
    areaKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredArea),
            Dimension(name = elementVar,
                      keys = c("5212", "5312")),
            Dimension(name = itemVar,
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )
    
    ## Query the data
    query = GetData(
        key = areaKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    
    query[, timePointYears := as.numeric(timePointYears)]
    setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    query
}


getCountryClassification = function(){
}

getCountryName = function(){
    tmp =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", list(code, description)]
    setnames(tmp, old = c("code", "description"),
             new = c("geographicAreaM49", "geographicAreaM49Name"))
    tmp
}

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



getWorldBankClimateData = function(){
    allCountries =
        GetCodeList(domain = "WorldBank",
                    dataset = "wb_ecogrw",
                    dimension = "geographicAreaM49")[type == "country", code]
    
    newKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_climate",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = allCountries),
                           Dimension(name = "wbIndicator",
                                     keys = c("SWS.FAO.PREC", "SWS.FAO.TEMP")),
                           Dimension(name = "timePointYears",
                                     keys = selectedYear)
                       )
                   )

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "wbIndicator", ascending = TRUE)        
    )

    climateData = GetData(key = newKey, pivoting = newPivot, normalized = FALSE)
    climateData[, timePointYears := as.numeric(timePointYears)]
    climateData
}

getStandardSeedRate = function(){
    seedGeneral.dt =
        GetTableData(schemaName = "ess", tableName = "default_seed_rate")
    ## NOTE (Michael): We again assume the rates are official here
    seedGeneral.dt[, flagObservationStatus_seedRate := ""]
    setnames(seedGeneral.dt,
             old = c("item", "value"),
             new = c("measuredItemCPC", "Value_seedRate"))
    setkeyv(seedGeneral.dt, "measuredItemCPC")
    seedGeneral.dt
}

mergeAllSeedData = function(seedData, ...){
    explanatoryData = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
    },
           x = explanatoryData, init = seedData
           )
}


removeCarryForward = function(data, variable){
    data[, variance := var(.SD[[variable]], na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(.SD[[variable]]),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}

requiredArea = getAllArea()
requiredItems = getAllItemCPC()
countryClassification = getCountryClassification()
countryNames = getCountryName()
wb = getWorldBankClimateData()
seed = getOfficialSeedData()
area = getAreaData()
stdSeedRate = getStandardSeedRate()
itemName = GetCodeList(domain = "agriculture", dataset = "agriculture", dimension = "measuredItemCPC")[, list(measuredItemCPC = code, measuredItemNameCPC = description)]



final = mergeAllSeedData(seedData = seed, area, countryNames, wb, stdSeedRate, itemName)
setnames(final,
         old = c("Value_measuredElement_5525", "Value_measuredElement_5212",
             "Value_measuredElement_5312", "Value_wbIndicator_SWS.FAO.TEMP",
             "Value_wbIndicator_SWS.FAO.PREC", "Value_seedRate"),
         new = c("seed", "areaSown", "areaHarvested", "temperature",
             "precipitation", "seedRate"))
## Create hierachy levels for CPC
final[, `:=`(c("cpclv1", "cpclv2", "cpclv3"),
             lapply(1:3,
                    FUN = function(x) substring(.SD$measuredItemCPC, 1, x)))]

final[, cpcLv3Name := itemName[match(cpclv3, itemName$measuredItemCPC),
                       measuredItemNameCPC]]

## Create factors
final[, `:=`(c("geographicAreaM49Name", "cpclv1", "cpclv2", "cpclv3",
               "measuredItemCPC"),
             lapply(c("geographicAreaM49Name", "cpclv1", "cpclv2", "cpclv3",
               "measuredItemCPC"),
                    FUN = function(x) factor(.SD[[x]])))]


seedRemoveCarryForward = removeCarryForward(final, "seed")
seedFinalData = subset(seedRemoveCarryForward,
    seed > 1 & areaHarvested > 1 & !measuredItemCPC %in% c("01802", "01921.01"))

##  Plots
xyplot(seed ~ areaHarvested, data = seedFinalData)
xyplot(log(seed) ~ log(areaHarvested), data = seedFinalData)

xyplot(log(seed) ~ log(areaHarvested)|geographicAreaM49Name * cpclv3, data = seedFinalData)

xyplot(log(seed) ~ temperature, data = seedFinalData)
xyplot(log(seed) ~ precipitation, data = seedFinalData)

xyplot(log(seed) ~ temperature|geographicAreaM49Name, data = seedFinalData)

xyplot(log(seed) ~ precipitation|geographicAreaM49Name, data = seedFinalData)



## Create training an test set
trainIndex = sample(NROW(seedFinalData), NROW(seedFinalData) * 0.75)
finalTrainData = seedFinalData[trainIndex, ]
finalTestData = seedFinalData[-trainIndex, ]


seedLmModel = lm(log(seed) ~ temperature + precipitation + timePointYears +
                     log(areaHarvested), data = finalTrainData)

## Need to add the hierachy for area
seedLmeModel = 
    lmer(log(seed) ~ temperature + timePointYears +
         (-1 + log(areaHarvested)|cpcLv3Name/measuredItemNameCPC/measuredItemNameCPC:geographicAreaM49Name),
         data = finalTrainData)

randomEffects = ranef(seedLmeModel)
pdf(file = "seedRates.pdf", width = 10, height = 15)
## print(dotplot(randomEffects)[[1]])
print(dotplot(randomEffects)[[2]])
print(dotplot(randomEffects)[[3]])
graphics.off()


## Plot the predicted with observed in the training data
finalTrainData$predicted = exp(predict(seedLmeModel, finalTrainData,
    allow.new.levels = TRUE))

## R squared
1 - with(finalTrainData, sum((predicted - seed)^2))/
    with(finalTrainData, sum((seed - mean(seed))^2))

par(mfrow = c(1, 2))
with(finalTrainData, plot(predicted, seed,
                 xlim = c(0, 1.5e7), ylim = c(0, 1.5e7)))
abline(a = 0, b = 1, col = "red", lty = 2)
with(finalTrainData, plot(log(predicted), log(seed), xlim = c(0, 20),
                          ylim = c(0, 20)))
abline(a = 0, b = 1, col = "red", lty = 2)


## Prediction on test data
finalTestData$predicted = exp(predict(seedLmeModel, finalTestData,
    allow.new.levels = TRUE))

## Plot the predicted with observed in the training data
par(mfrow = c(1, 2))
with(finalTestData, plot(predicted, seed,
                 xlim = c(0, 1.5e7), ylim = c(0, 1.5e7),
                         main = "predicted vs observed (Test)"))
abline(a = 0, b = 1, col = "red", lty = 2)

with(finalTestData, plot(log(predicted), log(seed), xlim = c(0, 20),
                          ylim = c(0, 20)))
abline(a = 0, b = 1, col = "red", lty = 2)
