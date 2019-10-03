# ---------------------------------------
# ---------------------------------------
# Snow hare data extraction
# ---------------------------------------
# ---------------------------------------

library(rgdal)
library(ncdf4)
library(ggplot2)


# ---------------------------------------
# Get indices

index = readOGR("~/Documents/CNP_snow/data/Cairngorms.gpkg", "forecast_grid")@data %>% 
   as_tibble()


# ---------------------------------------
# Extract from NetCDF

extractor = function(cell.x, cell.y, id){
   x = lapply(1960:2010, function(i){
      nc = nc_open(paste0("~/Documents/PhD/calcs/Snow_mod/results/grid/full_", i, ".nc"))
      y = ncvar_get(nc,
                    "SCA50",
                    start=c(cell.x, (cell.y), 1),
                    count=c(1, 1, -1))
      nc_close(nc)
      
      data.frame(date=seq.Date(
         as.Date(
            paste0(
               i, "-10-01")),
         by=1, length.out=length(y)
      ), round(y))
      
   })
   x = do.call("rbind.data.frame", x)
   colnames(x)[2] = "M50"
   x$cell_id = id
   x
}

# run
for(r in 1:nrow(index)){
   M50 = extractor(index$ncdf_x[r], index$ncdf_y[r], id = index$forecast_id[r])
   write.csv(M50, paste0("~/Documents/CNP_snow/results/M50_", index$forecast_id[r], ".csv"), row.names=F, quote=F)
}


# ---------------------------------------
# Check

f = list.files("~/Documents/CNP_snow/results", pattern="M50", full.names=T)
x = lapply(f[1:4], function(i){
   y = read.csv(i)
   y$cumulative = cumsum(y$M50)
   y
})
x = do.call("rbind.data.frame", x)

ggplot(x, aes(date, cumulative)) +
   geom_point(aes(colour=cell_id))
