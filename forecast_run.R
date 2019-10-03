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

f = list.files("../Data", full.names = T)
f = f[1:2]

# ---------------------------------------
# Read, clean and run

tic = proc.time()
cg = mclapply(f, mc.cores = 50, function(i){
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
   
   z = lapply(unique(x$model), function(j){
      # Model run
      y = model(st = filter(x, model == j))
      
      # Days per year
      y %>% 
         group_by(year, observation_id, model) %>% 
         summarise(M50 = sum(M50))
   })
   do.call("rbind.data.frame", z)
})
proc.time() - tic

cg = do.call("rbind.data.frame", cg)


# ---------------------------------------
# Write

write_csv(cg, "../forecast_run.csv")
