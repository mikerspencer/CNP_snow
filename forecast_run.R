# ---------------------------------------
# ---------------------------------------
# Forecast run
# ---------------------------------------
# ---------------------------------------

library(tidyverse)
library(parallel)
source("model.R")


# ---------------------------------------
# files

f = list.files("~/Documents/CNP_snow/data/forecast/Data", full.names = T)


# ---------------------------------------
# Read, clean and run

cg = lapply(f, function(i){
   # Data in
   x = read_csv(i) %>% 
      gather(var, value, -observation_id, -year, -doy) %>% 
      separate(var, c("var", "model")) %>% 
      spread(var, value) %>% 
      mutate(meantemp = (max + min) / 2) %>% 
      mutate(year = if_else(doy < 180, year - 1, year),
             dowy = if_else(doy < 181, doy + 180, doy - 180),
             Date = paste(year, dowy, sep = "_")) %>% 
      select(-min, -max, -doy, Precip = rainfall)
   
   z = mclapply(unique(x$model), mc.cores = 3, function(j){
      # Model run
      y = model(st = filter(x, model == j))
      
      # Days per year
      y %>% 
         group_by(year, observation_id, model) %>% 
         summarise(M50 = sum(M50))
   })
   do.call("rbind.data.frame", z)
})


# ---------------------------------------
# QA Plot

ggplot(z, aes(year, M50)) +
   geom_point(aes(colour = model)) +
   stat_smooth(SE = F)
