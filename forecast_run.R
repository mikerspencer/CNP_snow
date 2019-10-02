# ---------------------------------------
# ---------------------------------------
# Forecast run
# ---------------------------------------
# ---------------------------------------

library(tidyverse)
source("model.R")


# ---------------------------------------
# Data in

f = list.files("~/Documents/CNP_snow/data/forecast/Data", full.names = T)

x = read_csv(f[1]) %>% 
   gather(var, value, -observation_id, -year, -doy) %>% 
   separate(var, c("var", "model")) %>% 
   spread(var, value) %>% 
   mutate(meantemp = mean(c(max, min))) %>% 
   mutate(year = if_else(doy < 180, year - 1, year),
          dowy = if_else(doy < 181, doy + 180, doy - 180)) %>% 
   select(-min, -max, -doy, Precip = rainfall)


# ---------------------------------------
# Model run

y = model(x)
