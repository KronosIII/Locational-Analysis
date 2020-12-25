library(sf)
library(dplyr)
library(raster)
library(tmap)
#######################################################
bank.sf <- read_sf('Data/bank.shp')
streets.sf <- read_sf('Data/streets.shp')
pop.ras <- raster("Data/popden")
#######################################################
tm_shape(pop.ras) + tm_raster(palette = 'Greens') +
tm_shape(streets.sf) + tm_lines() +
tm_shape(bank.sf) + tm_dots(col='red', size=1)
#######################################################
crs(pop.ras)
crs.prj <- crs(pop.ras)
crs(bank.sf)
bank.sf2 <- bank.sf %>% st_set_crs(4326) %>% st_transform(crs.prj)
crs(streets.sf)
streets.sf2 = streets.sf %>% st_set_crs(4326) %>% st_transform(crs.prj)
#######################################################
# check the dimension of pop.ras (# of rows, # of columns)
pop.ras
r <- raster(extent(pop.ras), resolution = 30,
            crs = st_crs(pop.ras)$proj4string)
bank.ras = rasterize(bank.sf2, r, field = 1)
streets.ras = rasterize(streets.sf2,r, field =1)
dist_bank.ras <- distance(bank.ras)
# mapping the distance surface
plot(dist_bank.ras)
##########################################################
high_popden.ras <- (pop.ras>5000)
plot(high_popden.ras)
#########################################################
out1 = (pop.ras>5000)*(dist_bank.ras>500)
plot(out1)
plot(streets.ras, add = TRUE)
# merge(out1,streets)
# check the distribution of private deposits
summary(bank.sf2$PRIVAT_DEP)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 1.780e+05 1.303e+07 2.910e+07 1.470e+08 4.026e+07 1.928e+09
# crate an index whose bank account is 15000000, for an example.
ind <- (bank.sf2$PRIVAT_DEP > 25000000)
# visualize the selected sites
tm_shape(bank.sf2) + tm_dots(col='red',size=1)+
  tm_shape(bank.sf2[ind,]) + tm_dots(col='black',size=.1)


ind <- (bank.sf2$PRIVAT_DEP > 25000000)
##################################### visualize the selected sites
tm_shape(pop.ras) + tm_raster(palette = 'Greens') +
  tm_shape(streets.sf) + tm_lines() +
  tm_shape(bank.sf) + tm_dots(col='red', size=1)

tm_shape(out1) + tm_dots(col='red',size=1)+
  tm_shape(bank.sf2[ind,]) + tm_dots(col='black',size=.1)+tm_shape(streets.ras)

out1 = (pop.ras>7500)*(dist_bank.ras>250)

tm_shape(out1) + tm_raster(palette = 'Greens') +
  tm_shape(streets.ras) + tm_lines() +
  tm_shape(ind) + tm_dots(col='black',size=.1)