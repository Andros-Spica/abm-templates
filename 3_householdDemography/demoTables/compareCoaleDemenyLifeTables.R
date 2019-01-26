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

#this.dir <- dirname(parent.frame(2)$ofile) # r file must be "sourced" for this to work in RStudio
#setwd(this.dir)

# load external data
# ext.dt <- list(
#   F = read.table("table_womenMortalityTable.txt", skip = 2, nrows = 21, sep = " "),
#   M = read.table("table_menMortalityTable.txt", skip = 2, nrows = 21, sep = " ")
# )

# set parameters (for building dataset and plotting)
sex = "F"
level = 3
ages = 0:150
submodel.col <- c("red", "green", "blue", "purple", "black")
submodels.pch <- c(1, 2, 3, 4, 5)

# build dataset
dt <- data.frame(
  rbind(
    cbind(ages, rep("west",     151), 
          read.table(paste("cdmltw", sex, ".txt", sep = ""), sep = " ")[,level]
          ),
    cbind(ages, rep("east",     151),
          read.table(paste("cdmlte", sex, ".txt", sep = ""), sep = " ")[,level]
          ),
    cbind(ages, rep("south",    151), 
          read.table(paste("cdmlts", sex, ".txt", sep = ""), sep = " ")[,level]
          ),
    cbind(ages, rep("north",    151), 
          read.table(paste("cdmltn", sex, ".txt", sep = ""), sep = " ")[,level]
          )
  ))

names(dt) <- c("ages", "submodel", "nqx")

dt$submodel <- factor(dt$submodel, 
                      levels = c("west", "east", "south", "north"))

dt$ages <- as.numeric(as.character(dt$ages))

dt$nqx <- as.numeric(as.character(dt$nqx))


# plot
plot(dt$ages, 0:max(dt$nqx), 
     type="n", 
     xlab = "age cohort", ylab = "mortality (nqx)")

for (v in 2:ncol(dt)) {
  lines(dt$ages,
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


