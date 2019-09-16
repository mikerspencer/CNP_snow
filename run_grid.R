# ---------------------------------------
# ---------------------------------------
# Gridded model run
# ---------------------------------------
# ---------------------------------------

# ---------------------------------------
# Setup
# ---------------------------------------
setwd("~/Cloud/Michael/Uni_temp/Snow_mod")
#install.packages("ncdf4")
library(ncdf4)
library(parallel)
source("./repo/model.R")
source("./repo/functions.R")


# ---------------------------------------
# Sample test grid
# Ardtalnaig 1 year
# ---------------------------------------
# Spatial subset
nc.sample = data.frame(x=rep(47:57, by=12), y=rep(38:49, each=11))
# Dates
Dates = data.frame(year=1960:2010, day0=as.numeric(strftime(paste0(1960:2010, "-10-01"), "%j")), days1=as.numeric(strftime(paste0(1961:2011, "-05-31"), "%j")), days.total=as.numeric(as.Date(paste0(1961:2011, "-05-31")) - as.Date(paste0(1960:2010, "-10-01"))))

# Generate netcdf results containers
nc_creator(paste0(1981, "_ardtalnaig"), Dates[Dates$year==1981, 4] + 1)

# Loop through cells
lapply(1:nrow(nc.sample), function(i){
   # Read data
   x = nc_reader(1981, nc.sample[i, 1], nc.sample[i, 2], 274, 151)
   # Run model
   x.mod = model(x, 0.9, 6.6)
   # Write to netcdf
   nc_writer(paste0(1981, "_ardtalnaig.nc"), x.mod[, 4:5], x=nc.sample[i, 1], y=nc.sample[i, 2])
})


# ---------------------------------------
# Define grid as non NA vectors
# ---------------------------------------

nc.grid = data.frame(x=rep(1:140, by=144), y=rep(1:144, each=140))

# Sample first day of whole grid
nc = nc_open("./data/comb/temp_precip_1960.nc")
nc.na = lapply(1:nrow(nc.grid), function(i){
   d = ncvar_get(nc, "precip", start=c(nc.grid[i, 1], nc.grid[i, 2], 1), count=c(1, 1, 1))
})
# Test which are NA
nc.na = sapply(nc.na, is.na)

# Subset nc.grid with no NA values
nc.grid = nc.grid[!nc.na,]

# Get NGRs
nc.x = ncvar_get(nc, "x")
nc.y = sort(ncvar_get(nc, "y"))

# Join index with NGRs
nc.grid$east = nc.x[nc.grid$x]
nc.grid$north = nc.y[nc.grid$y]

# Write for future analysis
write.csv(nc.grid, "./data/nc_grid_index.csv", row.names=F)

# Tidy
nc_close(nc)
rm(nc.na, nc, nc.x, nc.y)


# ---------------------------------------
# Generate netcdf results containers
# ---------------------------------------

Dates = data.frame(year=1960:2010, day0=as.numeric(strftime(paste0(1960:2010, "-10-01"), "%j")), days1=as.numeric(strftime(paste0(1961:2011, "-05-31"), "%j")), days.total=as.numeric(as.Date(paste0(1961:2011, "-05-31")) - as.Date(paste0(1960:2010, "-10-01"))))

lapply(1:nrow(Dates), function(i){
   nc_creator(paste0("full_", Dates[i, 1]), Dates[i, 4] + 1)
})


# ---------------------------------------
# Full run
# ---------------------------------------

# Taken from the grid_calibration.R file
param = as.numeric(param.run)

# Start the clock!
ptm <- proc.time()
# Loop each year
mclapply(1:nrow(Dates), mc.cores=4, FUN=function(i){
   print(Dates[i, 1])
   # Loop each cell
   x=lapply(1:nrow(nc.grid), function(j){
      # Read data
      nc.data = nc_reader(Dates[i, 1], nc.grid[j, 1], nc.grid[j, 2], Dates[i, 2], Dates[i, 3])
      # Run model
      nc.model = 
         # Avoid missing values
         if (sum(is.na(nc.data$meantemp)) + sum(is.na(nc.data$Precip)) == 0) {
            # Run model
            model(nc.data, param[1], param[2], param[3], param[4], param[5], param[6])[, c("SWE", "Melt", "M50")]
         } else {
            # Generate NA if missing values found
            data.frame(SWE=rep(NA, times=nrow(nc.data)), Melt=rep(NA, times=nrow(nc.data)), M50=rep(NA, times=nrow(nc.data)))
         }
      # Write result to netcdf
      nc_writer(paste0("full_", Dates[i, 1], ".nc"), nc.model, x=nc.grid[j, 1], y=nc.grid[j, 2])
   })
})

# Stop the clock
proc.time() - ptm
rm(ptm)


