# ------------------------------------------
# Snow accumulation and melt model
# ------------------------------------------

model = function(st, temp.b=1, DDF=5.5, den=120, den.i=2.5, DDF.d=0.01, d.50=100){
   # st = precip and temp data
   # temp.b = temperature threshold for rain/snow
   # DDF = degree day factor
   # den = initial snow density
   # den.i = density increase (as (end-den)/(days-day0))
   # DDF.d = DDF decreaser for slower melt from more dense snow (as (DDF-end)/(end-den))
   # d.50 = depth (mm) at which cover is greater than 50%
# Results container
   d = data.frame(Date = character(nrow(st)+1),
                  SWE = numeric(nrow(st)+1),
                  Melt = numeric(nrow(st)+1),
                  PrecipEff = numeric(nrow(st)+1))
# Warmup row
   d$Date = as.character(c(st$Date[1]-1, st$Date))
# Time steps
   for (i in 1:nrow(st)){
      j = i+1
# Density estimate
      # 150 kg/m3 fresh, 250 kg/m3 after 30 days
      y = rep(0, length=nrow(d))
      y[d$SWE > 0] = 1
      # Count days with snow lying, reset at 0
      d$days = y * ave(y, c(0L, cumsum(diff(y) != 0)), FUN=seq_along)
      # Make density a function of duration
      # den.i(250-den)/(30-1)
      d$density = round(den.i * d$days + (den - den.i))
      d$density[d$density<den] = 0
      if (st$meantemp[i] < temp.b){
# Accumulation model
         d$SWE[j] = d$SWE[j-1] + st$Precip[i]
         d$PrecipEff[j] = 0
         d$Melt[j] = 0
      } else {
# Melt model
         # DDF reduced when density higher
		   pot.melt = 
		      round(DDF.d * d$density[i] + (DDF - DDF.d)) * (st$meantemp[i] - temp.b)
         # More SWE than melt
         if (pot.melt < d$SWE[j-1]){
		      d$SWE[j] = d$SWE[j-1] - pot.melt
            d$Melt[j] = pot.melt
         # No SWE
         } else if (d$SWE[j-1] == 0) {
            d$SWE[j] = 0
            d$Melt[j] = 0
         # Less SWE than melt, but more SWE than 0
         } else {
            d$SWE[j] = 0
            d$Melt[j] = d$SWE[j-1]
         }
		   d$PrecipEff[j] = st$Precip[i] + d$Melt[j]
      }
   }
   d$Date = as.Date(d$Date)
   d = merge(st, d, by="Date", all.x=T)
   # Add model snow/no snow
   d$Model = 0
   d$Model[d$SWE>0] = 1
   # Add snow depth (mm)
   x = 10 / (d$density / 100)
   d$depth = round(d$SWE * x)
   d$depth[is.nan(d$depth)] = 0
   # Snow cover > 50%
   d$M50 = 0
   d$M50[d$depth>d.50] = 1
   d
}
