######################################################################
# Script to compare 'nqx' (probabilities of dying between age n and x)
# from Coale-Demeny Model Life Tables generated with 'demoR' package
# demoR package version 0.6.0 (2018-09-13)
# by James Holland Jones and collaborators
# and external life tables calculated with empirical data
# # Their source:
# Coale, A., P. Demeny, and B. Vaughn. 1983. 
# Regional model life tables and stable populations. 
# 2nd ed. New York: Academic Press.
######################################################################

#install.packages("demogR")

library(demogR)

this.dir <- dirname(parent.frame(2)$ofile) # r file must be "sourced" for this to work in RStudio
setwd(this.dir)

# load external data
ext.dt <- list(
  F = read.table("table_womenMortalityTable.txt", skip = 2, nrows = 21, sep = " "),
  M = read.table("table_menMortalityTable.txt", skip = 2, nrows = 21, sep = " ")
)

# set parameters (for building dataset and plotting)
sex = "F"
level = 3
ageCohort = c(0, 1, seq(5, 95, by = 5))
submodel.col <- c("red", "green", "blue", "purple", "black")
submodels.pch <- c(1, 2, 3, 4, 5)

# build dataset
dt <- data.frame(
  cbind(
    cbind(ageCohort, rep("west",     21), cdmltw(sex = sex)$nqx[level,]),
    cbind(ageCohort, rep("east",     21), cdmlte(sex = sex)$nqx[level,]),
    cbind(ageCohort, rep("south",    21), cdmlts(sex = sex)$nqx[level,]),
    cbind(ageCohort, rep("north",    21), cdmltn(sex = sex)$nqx[level,]),
    cbind(ageCohort, rep("ext.data", 21), ext.dt[[sex]][,2])
  ))

names(dt) <- c("ageCohort", "submodel", "nqx")

dt$submodel <- factor(dt$submodel, 
                      levels = c("west", "east", "south", "north", "ext.data"))

dt$ageCohort <- as.numeric(as.character(dt$ageCohort))

dt <- data.frame(
  cbind(
    ageCohort,
    "west" = cdmltw(sex = sex)$nqx[level,],
    "east" = cdmlte(sex = sex)$nqx[level,],
    "south" = cdmlts(sex = sex)$nqx[level,],
    "north" = cdmltn(sex = sex)$nqx[level,],
    "ext.data" = ext.dt[[sex]][,2]
  ))

# plot
plot(dt$ageCohort, 0:max(dt[,2:ncol(dt)]), 
     type="n", 
     xlab = "age cohort", ylab = "mortality (nqx)")

for (v in 2:ncol(dt)) {
  lines(dt$ageCohort,
        dt[v], 
        col = submodel.col[v], pch=submodels.pch[v],
        type = "b")
}

title(paste("Age-specific mortality rates (", sex, ", ", level, ")"))

legend(80, 40, 
       names(dt)[2:ncol(dt)], 
       col=submodel.col, pch=submodels.pch,
       lty = 1,
       title="submodel")


