######################################################################
# Script to interpolate age-specific demography rates imported from files
# (multi-age group or cohorts -> single age group or cohort) 
######################################################################

this.dir <- dirname(parent.frame(2)$ofile) # r file must be "sourced" for this to work in RStudio
setwd(this.dir)

# fertility (women)

fert <- read.table("table_womenFertilityTable_raw.txt", skip = 2, nrows = 7, sep = " ")

fert[,1] <- seq(17.5, 47.5, 5)

fert <- approx(fert[,1], fert[,2], xout = 1:150, yleft = 0, yright = 0)

write.table(fert, file = "table_womenFertilityTable_1year.txt", row.names = FALSE, col.names = FALSE)


# nuptiality (women & men)

w.nup <- read.table("table_womenNuptialityTable_raw.txt", skip = 2, nrows = 9, sep = " ")

w.nup[,1] <- seq(2.5, 42.5, 5)

w.nup <- approx(w.nup[,1], w.nup[,2], xout = 1:150, yleft = 0, yright = 0)

write.table(w.nup, file = "table_womenNuptialityTable_1year.txt", row.names = FALSE, col.names = FALSE)


m.nup <- read.table("table_menNuptialityTable_raw.txt", skip = 2, nrows = 9, sep = " ")

m.nup[,1] <- seq(2.5, 42.5, 5)

m.nup <- approx(m.nup[,1], m.nup[,2], xout = 1:150, yleft = 0, yright = 0)

write.table(m.nup, file = "table_menNuptialityTable_1year.txt", row.names = FALSE, col.names = FALSE)


# mortality (women & men)

w.mort <- read.table("table_womenMortalityTable_raw.txt", skip = 2, sep = " ")

w.mort[,1] <- c(0.5, 1.5, 4, seq(8.5, 98.5, 5))

w.mort <- approx(w.mort[,1], w.mort[,2], xout = 1:150, yleft = 0, yright = 1)

write.table(w.mort, file = "table_womenMortalityTable_1year.txt", row.names = FALSE, col.names = FALSE)


m.mort <- read.table("table_menMortalityTable_raw.txt", skip = 2, sep = " ")

m.mort[,1] <- c(0.5, 1.5, 4, seq(8.5, 98.5, 5))

m.mort <- approx(m.mort[,1], m.mort[,2], xout = 1:150, yleft = 0, yright = 1)

write.table(m.mort, file = "table_menMortalityTable_1year.txt", row.names = FALSE, col.names = FALSE)
