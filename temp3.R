## subplots are (theretically) 10 m side squares
## point coordinates a referred to subplot origin
## (actually 694/1848 points have a coordinate > 1000 cm: WHY?)
## to compute plot coordiantes origin coordinates are defined

ppwm %$% ppwm[x>1000 | y > 1000 ,1:6] %>% nrow()

side <- 1:5
subplot_origin <- expand.grid(c=side, r=side) %>%
  mutate(subplot = paste(LETTERS[c], r, sep=""),
         x0 = 1000 * (c - 1),
         y0 = 1000 * (r - 1))

ppwm2 <- ppwm %>%
  full_join(subplot_origin) %>%
  mutate(plot_x = (x0 + x)/100, 
         plot_y = (y0 + y)/100,
         dbh = ifelse(is.na(dbhEO), dbhNS, sqrt(mean(dbhNS^2, dbhEO^2)))
         )
