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

extractor = function(cell.x, cell.y){
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
   x
}

# run
for(r in 1:nrow(index)){
   M50 = extractor(index$ncdf_x[r], index$ncdf_y[r])
   write.csv(M50, paste0("~/Documents/CNP_snow/results/M50_", index[r, 1], ".csv"), row.names=F, quote=F)
}


# ---------------------------------------
# Check

f = list.files("~/Downloads/temp", pattern="SWE", full.names=T)
x = lapply(f, function(i){
   y = read.csv(i)
   y$name = strsplit(i, "/")[[1]][6]
   y$cumulative = cumsum(y$swe)
   y
})

ggplot(x, aes(date, cumulative)) +
   geom_point(aes(colour=name))


f = list.files("~/Downloads/temp", pattern="temp_", full.names=T)
x = lapply(f, function(i){
   y = read.csv(i)
   y$name = strsplit(i, "/")[[1]][6]
   y
})

x = do.call("rbind.data.frame", x)
x$date = as.Date(x$date)
x$Julian = format(x$date, "%J")

ggplot(x, aes(date, temp)) +
   geom_point(aes(colour=name))

ggplot(x, aes(Julian, temp)) +
   geom_point(aes(colour=name))
