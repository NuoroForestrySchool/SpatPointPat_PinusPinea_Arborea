library(tidyverse)
library(googlesheets)
library(spatstat)

suppressMessages(library(dplyr))
gs_ls()
tpp <- gs_title("TransectPinusPinea")
gs_ws_ls(tpp)
ppwm <- tpp %>% gs_read(ws="PointPatternWithMarks")
ppwm$nPf <- as.factor(ppwm$`N palchi`)


ppp_hf <- ppwm %>%
  group_by(Area) %>%
  #   do(create_ppp(., marks=.[,7:16])) # non sono riuscito a farlo funzionare, 
  do(ppp = ppp(.$x, .$y
               , window = ripras(.$x, .$y, shape = "rectangle")
               , marks = .[,7:16])) %>%
  as.hyperframe()
# source of insipiration: https://cran.r-project.org/web/packages/spatstat/vignettes/replicated.pdf

summary(ppp_hf$ppp)
df <- data.frame()
for(p in 1:nrow(ppp_hf)) df <- 
  rbind(df,data.frame(plot=ppp_hf$plot[[p]],
                  intensity=summary(ppp_hf$ppp[[p]])$intensity))
df

mark <- 4
mark.name <- colnames(ppp_hf$ppp$`1`$marks)[mark]
plot(ppp_hf$ppp, main = paste("Map of '", mark.name,"'", sep=""), 
     which.marks = mark, main.panel = ppp_hf$plot)

