require(here)
dat_habitat= read.csv(here("data","hab.sta.csv"))
dat_habitat
par(mfrow=c(3,1))
hist(dat_habitat$elev, main="Elevation Terrain",xlab="Elevation (m)",ylab="Frequency")
hist(dat_habitat$aspect, main="Aspect Terrain",xlab="Aspect (degrees)",ylab="Frequency")
hist(dat_habitat$slope, main="Slope Terrain",xlab="Slope (%)",ylab="Frequency")
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
par(mfrow=c(3,1))
plot(x=dat_habitat$elev,y=dat_habitat$ba.tot,main="Elevation vs Total Basal Area",xlab="Elevation",ylab= "Total Basal Area")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.09), add = TRUE) 
plot(x=dat_habitat$slope,y=dat_habitat$ba.tot,main="Slope vs Total Basal Area",xlab="Slope",ylab= "Total Basal Area")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.35), add = TRUE) 
plot(x=dat_habitat$aspect,y=dat_habitat$ba.tot,main="Aspect vs Total Basal Area",xlab="Aspect",ylab= "Total Basal Area")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.10), add = TRUE) 
