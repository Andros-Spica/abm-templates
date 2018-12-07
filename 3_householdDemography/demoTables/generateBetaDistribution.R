######################################################################
# Script to generate a standard shape of the age-specific fertility rate
# 'ASFR' (probabilities for a woman to give birth between at age i)
# from a beta distribution
# approach taken from Peter Ellis
# http://freerangestats.info/blog/2018/06/26/fertility-rate
# https://github.com/ellisp/blog-source/tree/master/_working/0122-demographics
######################################################################

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


fertile_ages <- 12:50
n_fertile_ages <- length(fertile_ages)

prob_birth <- c(
  rep(0, 12), 
  dbeta((fertile_ages - 12) / n_fertile_ages, 2, 4),
  rep(0, 49)
)

plot(prob_birth)

write.table(prob_birth, file = "betaDist.txt")
