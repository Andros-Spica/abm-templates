
# nuptiality

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

png("nuptialityModel.png", width = 800, height = 480)
par(cex = 2)

plot(c(1, 100), c(0, 1), type = "n", 
     main = "parametric model of nuptiality ", cex.main = 2,
     xlab = "AGE", cex.lab = 1.2,
     ylab = "p(x)"
)
lines(1:100, generateParametricModelNuptiality(), col = "red", lwd = 3)
lines(1:100, generateParametricModelNuptiality(par.c1 = .8, par.mu = 23, par.sigma = c(3,12)), col = "blue", lwd = 3)
text(70, 0.8, 
     expression(
       paste(
         italic("if "), x < mu, ", ", sigma == sigma[1], italic(" else "), sigma == sigma[2]
         )
       ))
text(70, 0.5,
     expression(
       p(x) == c[1]*italic(e)^-(frac(x - mu, sigma))^2
       )
     , cex = 1.2)

dev.off()

# mortality

interpolatePerYear <- function(raw, ages = c(0.5, 1.5, 4, seq(8.5, 93.5, 5))) {
  
  perYear <- data.frame(matrix(numeric(0), nrow = 151, ncol = ncol(raw)))
  names(perYear) <- 1:ncol(perYear)
  
  for (i in 1:ncol(raw)) {
    perYear[, i] <- approx(ages, raw[, i], 
                           xout = 1:151, yleft = 0, yright = 1)$y
  }
  row.names(perYear) <- 0:150
  
  return(perYear)
}

generateCoaleDemenyLifeTable <- function(region = "north", sex = "F", level = 8){
  
  curve <- 0
  
  if (region == "north")
  {
    curve <- interpolatePerYear(t(demogR::cdmltn(sex = sex)$nqx))[,level]
  }
  if (region == "west")
  {
    curve <- interpolatePerYear(t(demogR::cdmltw(sex = sex)$nqx))[,level]
  }
  if (region == "east")
  {
    curve <- interpolatePerYear(t(demogR::cdmlte(sex = sex)$nqx))[,level]
  }
  if (region == "south")
  {
    curve <- interpolatePerYear(t(demogR::cdmlts(sex = sex)$nqx))[,level]
  }
  
  return(curve)
}

png("mortalityModel-levels.png", width = 800, height = 480)
par(cex = 2)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality - levels (north)", cex.main = 1.5,
     xlab = "AGE", cex.lab = 1.5,
     ylab = "q(x)",
     xlim = c(0, 100)
)
lines(1:151, generateCoaleDemenyLifeTable(level = 1), col = "blue", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(level = 25), col = "darkgreen", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(level = 8), col = "red", lwd = 3)

dev.off()

png("mortalityModel-regions.png", width = 800, height = 480)
par(cex = 2)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality - regions (level 8)", cex.main = 1.5,
     xlab = "AGE", cex.lab = 1.5,
     ylab = "q(x)",
     xlim = c(0, 100)
)
lines(1:151, generateCoaleDemenyLifeTable(), col = "blue", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "west"), col = "darkgreen", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "east"), col = "red", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "south"), col = "purple", lwd = 3)

legend(0, 1, 
       c("north", "west", "east", "south"), 
       col = c("blue", "darkgreen", "red", "purple"),
       lwd = 3,
       title = "region")

dev.off()
