##' Function to compile seed.
##'
##' This function compiles seed rates from official data and imputes
##' them where missing using default rates . E.g. where seed rates are
##' missing for rye, it will apply the rates from the group
##' "root.crops"
##' 
##' @param sYr - from which year to begin processing
##' @param lYr - from which year to end processing
##' @param Refa - standard column references for output
##' @param seed - long format output file containing official seed rates
##' @param AllSeed - file containing default seed rates
##' @param AHdf - data.frame of area harvested taken from server 
##' @param SeedGroup - file containing group class of seeds
##' @param PrimaryAreaCodes - data.frame of nutrient and commodity
##' descriptor codes by country/year
##' 
##' @keywords seed
##' @export
##' @examples
##' fun.DU.SEED()

fun.DU.SEED = function (sYr, lYr,
    Refa,
    seed,
    AllSeed,
    AHdf,
    SeedGroup,
    PrimaryAreaCodes){
    
    ## replace "China, mainland (41)" with missing "China (351)" as
    ## China, mainland will dominate seed rates for all of China
    if("351" %in% seed$AreaCode == FALSE){
        seed[seed$AreaCode == 41, "AreaCode" ] <- 351
        seed[seed$AreaCode == 41, "AreaName" ] <- "China"
    }
    
    ## get the existing seed rates 
    xSeed <- merge(seed, SeedGroup, by = c("Item"), all = FALSE)
    xSeed <- xSeed[c("AreaCode", "Item", "ItemCode", "SeedRate")]

    ## merge with potential seed
    potentialSeed <- merge(xSeed, AllSeed, by = c("ItemCode"), all = TRUE)

    print("This is potentialSeed")
    print(str(potentialSeed))
    ## identify those returned NA as well as missing country rates and fill1
    missingSEED <- which(is.na(potentialSeed$SeedRate))
    complete.df <- potentialSeed[-missingSEED, ]
    missing.df <- potentialSeed[missingSEED, c("ItemCode", "item", "seed.rate")]
    seedAreas.df <- unique(complete.df$AreaCode)
    toFill.df <- merge(seedAreas.df, missing.df, 
                       by = NULL, all = TRUE)
    print("This is toFill.df")
    print(str(toFill.df))
    colnames(toFill.df)[1] <- "AreaCode"
    tmpNames <- c("AreaCode", "ItemCode", "Item", "SeedRate")
    complete.df <- complete.df[ , c(tmpNames)]
    colnames(complete.df) <- colnames(toFill.df) <- tmpNames
    completed.df <- rbind(complete.df, toFill.df)

    ## format file and get nutrients for seeds
    AHdf$Flag <- NULL
    colnames(AHdf)[colnames(AHdf) == "Value"] <- "Area"
    
    lSEED <- merge(completed.df, AHdf, 
                   by = c("AreaCode", "ItemCode"), all = FALSE)
    lSEED <- lSEED[lSEED$Year >= sYr & lSEED$Year <= lYr, ]
    
    lSEED <- merge(unique(lSEED), PrimaryAreaCodes, 
                   by = c("ItemCode", "AreaCode", "Year"), 
                   all = FALSE)
    print("This is lSEED")
    print(str(lSEED))
    
    lSEED$Item <- lSEED$ElementCode <- lSEED$CPC <- lSEED$Items.. <- NULL

    ##multiply be area to get quantities of area seeded
    lSEED$Area <- as.numeric(as.character(lSEED$Area))
    lSEED$SeedRate <- as.numeric(as.character(lSEED$SeedRate))
    lSEED["SEED"] <- lSEED$SeedRate / 1000 * lSEED$Area
    lSEED["SEEDe"] <- lSEED$SeedRate / 1000 * lSEED$Area * lSEED$ENERC_KCAL
    lSEED["SEEDp"] <- lSEED$SeedRate / 1000 * lSEED$Area * lSEED$Protein / 100
    lSEED["SEEDf"] <- lSEED$SeedRate / 1000 * lSEED$Area * lSEED$Lipid_Tot / 100
    lSEED["SEEDc"] <- lSEED$SeedRate / 1000 * lSEED$Area * lSEED$CHOCDF / 100

    RefSeed <- c("SEED", "SEEDe", "SEEDp", "SEEDf", "SEEDc")
    
    ##aggregate by FBS commodity list  
    laggregatedSEED = aggregate(x = lSEED[RefSeed], 
        by = list(AreaCode=lSEED$AreaCode, Group=lSEED$Group, 
            Commodity=lSEED$Commodity, Year=lSEED$Year), 
        FUN = sum)

    laggregatedSEED <- laggregatedSEED[ ,c(Refa, RefSeed)]
    lSEED <- laggregatedSEED
    
    return(lSEED)	
}
