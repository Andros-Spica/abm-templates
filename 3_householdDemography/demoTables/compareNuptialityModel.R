# The following correspond to the first parametric model
# for fitting the age-specific distributions of marriages, mentioned in page 133 of:
# # Peristeva and Kostaki
# "A parametric model for estimating nuptiality patterns in modern populations"
# Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].

generateParametricModelNuptiality <- function(par.c1 = 0.85, par.mu = 20, par.sigma = c(5, 10) ){
  
  curve <- c()
  for (i in 1:100)
  {
    sigma = par.sigma[2]
    if (i < par.mu)
    { sigma = par.sigma[1] }
    
    curve <- c(curve, par.c1 * exp (-1 * (((i - par.mu) / sigma) ^ 2)) )
  }
 
  return(curve)
}

plot(c(1, 100), c(0, 1), type = "n")
lines(1:100, generateParametricModelNuptiality(), col = "black")
lines(1:100, generateParametricModelNuptiality(par.c1 = 0.5), col = "red")
lines(1:100, generateParametricModelNuptiality(par.mu = 15), col = "blue")
lines(1:100, generateParametricModelNuptiality(par.sigma = c(2, 20)), col = "darkgreen")


# generateDoubleExp <- function(par.E = 0.85, par.mu = 20, par.sigma = 5){
#   return(
#     (par.E / par.sigma) * 1.2813 * exp (-1.145 * (((1:100 - par.mu) / par.sigma) + 0.805) - exp (-1.896 * (((1:100 - par.mu) / par.sigma) + 0.805)) )
#   )
# }