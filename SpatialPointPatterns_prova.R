subplotcoord <- function(str) {
  return( list( (strtoi(charToRaw(substring(toupper(str), 1, 1)))-41)*1000, 
                1000*(strtoi(substring(str, 2, 2))-1) ))
}


library(googlesheets)
suppressMessages(library(dplyr))
gs_ls()
tpp <- gs_title("DatiConcas")
gs_ws_ls(tpp)
ppwm <- tpp %>% gs_read(ws="Foglio1")

ppwm$species <- toupper(ppwm$species)

ppwm <- ppwm[ppwm$species=="PINPIN", ]

ppwm$`h_tot`[is.na(ppwm$`h_tot`)==TRUE] <- 0
ppwm$`age`[is.na(ppwm$`age`)==TRUE] <- 0

ppwm$nPf <- as.factor(ppwm$`age`)

ppwm$`H [m]` <- ppwm$`h_tot`/100
ppwm$`sum_d130sq` <- (ppwm$`dbhNS`)^2

ppwm$`sum_d130sq`[is.na(ppwm$`sum_d130sq`)==TRUE] <- 0

ppwm$`crown_rE`[is.na(ppwm$`crown_rE`)==TRUE] <- 0
ppwm$`crown_rO`[is.na(ppwm$`crown_rO`)==TRUE] <- 0
ppwm$`crown_rN`[is.na(ppwm$`crown_rN`)==TRUE] <- 0
ppwm$`crown_rS`[is.na(ppwm$`crown_rS`)==TRUE] <- 0

ppwm$"AreaInsid" = pi*(((ppwm$"crown_rN"+ppwm$"crown_rE"+ppwm$"crown_rS"+ppwm$"crown_rO")/4)^2)

for(row in 1:lengths(ppwm)["plot"]) {
  plotcoord = subplotcoord(ppwm[row, "subplot"])
  ppwm[row, "xm"] = ppwm[row, "x"]+  plotcoord[1]
  ppwm[row, "ym"] = ppwm[row, "y"]+  plotcoord[2]
}

require(sqldf)
#t_windows0 <- sqldf("select Area, min(X) Xmin, max(X) Xmax from ppwm group by Area", drv='SQLite')

library(spatstat)
t_windows <- list()
for(tid in unique(ppwm$plot)) t_windows[[tid]] <- ripras(ppwm$xm[ppwm$plot==tid],ppwm$ym[ppwm$plot==tid], shape = "rectangle")
Areas <- unique(ppwm$plot)

tid <- Areas[2]
trs <- ppp(ppwm$xm[ppwm$plot==tid], ppwm$ym[ppwm$plot==tid]
           , xrange = t_windows[[tid]]$xrange, yrange = c(0, 6000)
           , marks = ppwm[ppwm$plot==tid, c("H [m]", "age", "AreaInsid", "species", "sum_d130sq", "nPf")])
# , unlist(t_windows[t_windows$Area==tid,c('X0', 'X1')]), c(-10-marg, 10+marg)
unitname(trs) <- "cm"
plot(trs, arrange=FALSE, par(mar=rep(3,4)))
summary(trs)$intensity
side <- 1000 # m - tessellation step
nx <- floor((trs$window$xrange[2] - trs$window$xrange[1])/side)
ny <- floor((trs$window$yrange[2] - trs$window$yrange[1])/side)
Q <- quadratcount(trs, nx, ny)
plot(trs, arrange=FALSE,cex=.5, pch="+")
plot(Q, add=TRUE, cex=.5)
# qua
den <- density(trs, bw = "sj", adjust = 2) # , sigma=70)
plot(den)
plot(trs, add=TRUE, arrange=FALSE,cex=.5, pch="+")

M <- quadrat.test(trs, nx=5, ny=1)
plot(trs, arrange=FALSE,cex=.5, pch="+")
plot(M, add=TRUE)
M

trsnm <- trs
marks(trsnm) <- NULL
(fit <- ppm(trsnm, ~polynom(x, 2)))
plot(fit, how="image")
plot(predict(fit, type = "trend"))

fitnull <- update(fit, ~1)
anova(fitnull, fit, test = "Chi")

plot(rmh(fit), main = "Simulazione")

(Mf <- quadrat.test(fit, 5, 1))

diagnose.ppm(fit, which = "smooth")

qqplot.ppm(fit)

class(trs$marks) <- "data.frame"

plot(split(cut(trs, "age", breaks=2)), ncols=1)

marktable(setmarks(trsnm,marks(trs)$nPf), N=1, collapse=T)  # N=2 non semba diverso, ma neanche meglio
image(marktable(setmarks(trsnm,marks(trs)$nPf), N=1, exclude=T, collapse=T))
contour((marktable(setmarks(trsnm,marks(trs)$nPf), N=1, exclude=T, collapse=T)))

trsnm <- trs
marks(trsnm) <- NULL
marks(trsnm) <- trs$marks[1:3]

mcf <- function(m1, m2) {m1*m2}
plot(markcorr(trsnm, mcf))
plot(markcorr(trsnm))
plot(markvario(trsnm))
plot(markcrosscorr(trsnm))
