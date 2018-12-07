######################################################################
# Script to export 'nqx' (probabilities of dying between age n and x)
# from Coale-Demeny Model Life Tables generated with 'demoR' package
# demoR package version 0.6.0 (2018-09-13)
# by James Holland Jones and collaborators
# Their source:
# Coale, A., P. Demeny, and B. Vaughn. 1983. 
# Regional model life tables and stable populations. 
# 2nd ed. New York: Academic Press.
######################################################################

#install.packages("demogR")

library(demogR)

this.dir <- dirname(parent.frame(2)$ofile) # r file must be "sourced" for this to work in RStudio
setwd(this.dir)

ages <- c(0.5, 1.5, 4, seq(8.5, 98.5, 5))

# West

west_raw <- t(cdmltw(sex = "F")$nqx)

west_1year <- data.frame()

for (i in 1:ncol(west)) {
  west_1year <- cbind(
    west_1year,
    "1" = 
  )
}
fert[,1] <- seq(17.5, 47.5, 5)

fert <- approx(fert[,1], fert[,2], xout = 1:150, yleft = 0, yright = 0)

write.table(cdmltw(sex = "F")$nqx, file = "cdmltwF.txt")

write.table(cdmltw(sex = "M")$nqx, file = "cdmltwM.txt")

# East

write.table(cdmlte(sex = "F")$nqx, file = "cdmlteF.txt")

write.table(cdmlte(sex = "M")$nqx, file = "cdmlteM.txt")

# South

write.table(cdmlts(sex = "F")$nqx, file = "cdmltsF.txt")

write.table(cdmlts(sex = "M")$nqx, file = "cdmltsM.txt")

# North

write.table(cdmltn(sex = "F")$nqx, file = "cdmltnF.txt")

write.table(cdmltn(sex = "M")$nqx, file = "cdmltnM.txt")
