
sYr <- 2007 # start year
lYr <- 2012 # end year

Refa <- c("AreaCode", "Group", "Commodity", "Year") #standard column references for output
seed <- read.csv("seed.csv", header = TRUE, sep = ",")
AllSeed <- read.csv("AllSeed.csv", header = TRUE, sep = ",")
AHdf <- read.csv("AllArea.csv", header = TRUE, sep = ",")
SeedGroup <- read.csv("seedGroup.csv", header = TRUE, sep = ",")
PrimaryAreaCodes <- read.csv("PrimaryAreaCodes.csv", header = TRUE, sep = ",")




DU.SEED.dat <- fun.DU.SEED(sYr, lYr,
                           Refa,
                           seed,
                           AllSeed,
                           AHdf,
                           SeedGroup,
                           PrimaryAreaCodes)

