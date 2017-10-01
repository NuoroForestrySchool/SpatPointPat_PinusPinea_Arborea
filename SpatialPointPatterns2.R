library("spatstat", lib.loc="~/R/win-library/3.3")
data(bei)              # pag 106
fit <- ppm(bei, ~elev + grad, covariates = bei.extra)
lam <- predict(fit, locations = bei)
Ki <- Kinhom(bei, lam)
plot(Ki, main = "Inhomogeneous K function")
g <- pcf(Kinhom(bei))
plot(g)             # pag 106
plot(pcf(Kinhom(bei, lam)))  # aggiunta mia

marks(trsnm) <- NULL
fit <- ppm(trsnm, ~polynom(x, 2))
lam <- predict(fit, locations = trsnm)

# F-function: empty space function = = expected n.of points within dist. r from any given position


# K-function: lambda*K(r) = expected n.of points within dist. r from a typical point
# pag. 92  (K(r)> PI*r^2 ==> clustering,  < ==> regular pattern)
plot(Kinhom(trsnm, lam))
plot(pcf(Kinhom(trsnm, lam)))
  
# pag. 106
# ginhom(r) is the probability of observing a pair of points at certain locations
# separated by a distance r, divided by the corresponding probability
# for a Poisson process of the same (inhomogeneous) intensity
# The inhomogeneous pair correlation function is currently computed by calling Kinhom followed
# by pcf.fv (which does numerical differentiation):
# pag. 94 ,
# This is a non-centred correlation which may
# take any nonnegative value. The value g(r) = 1 corresponds to complete randomness; for the
# Poisson process the pair correlation is gpois(r) ≡ 1. For other processes, values g(r) > 1 suggest
# clustering or attraction at distance r, while values g(r) < 1 suggest inhibition or regularity.
