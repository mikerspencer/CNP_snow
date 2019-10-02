# ---------------------------------------
# ---------------------------------------
# Forecast run
# ---------------------------------------
# ---------------------------------------

library(tidyverse)
source("model.R")


# ---------------------------------------
# Data

f = list.files("~/Documents/CNP_snow/data/forecast/Data", full.names = T)

read_csv(f[1]) %>% 
   gather(var, value, -observation_id, -year, -doy) %>% 
   separate(var, c("var", "model")) %>% 
   spread(var, value) %>% 
   mutate(temp = mean(c(max, min))) %>% 
   select(-min, -max)
