library(tidyverse)
library(googlesheets)
library(spatstat)

suppressMessages(library(dplyr))
gs_ls()
tpp <- gs_title("TransectPinusPinea")
gs_ws_ls(tpp)
ppwm <- tpp %>% gs_read(ws="PointPatternWithMarks")
ppwm$nPf <- as.factor(ppwm$`N palchi`)

# non funziona: il 'ppp' restituito viene rifiutato
# Serviva per aggiungere: unitname(ppp) <- "m"
create_ppp <-function(tally_sheet, marks = NULL, ...) {
  ppp <- tally_sheet %$%
    ppp(x, y, window = ripras(x, y, shape = "rectangle")
            , marks = marks)
  unitname(ppp) <- "m"
  return( data.frame(ppp=list(ppp)))
}

# debugonce(create_ppp)

ppp_hf <- ppwm %>%
  group_by(plot) %>%
  #   do(create_ppp(., marks=.[,7:16])) # non sono riuscito a farlo funzionare, 
  do(ppp = ppp(.$x, .$y
               , window = ripras(.$x, .$y, shape = "rectangle")
               , marks = .[,7:16])) %>%
  as.hyperframe()
# source of insipiration: https://cran.r-project.org/web/packages/spatstat/vignettes/replicated.pdf

summary(ppp_hf$ppp)

mark <- 4
mark.name <- colnames(ppp_hf$ppp$`1`$marks)[mark]
plot(ppp_hf$ppp, main = paste("Map of '", mark.name,"'", sep=""), 
     which.marks = mark, main.panel = ppp_hf$plot)

