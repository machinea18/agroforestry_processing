library(terra)
NI_parcels <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/BNG.gpkg")

results <- data.frame(CORINE = rep(NA, length(NI_parcels)), ODSE = NA,
                      OSM = NA, TCD10m = NA, TCD100m = NA,
                      SWF = NA, ETH_CHM = NA, area = NA)

#reading
results <- readRDS("prelim_results2.rds")
#rasters
CORINE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/CORINE_2018.tif")
ODSE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/ODSE_2020.tif")
OSMLU <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc//OSMLU/OSMLU.tif")
UKCEH <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/UKCEH.tif")
#trees
TCD10m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree10m2.tif")
TCD100m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree100m2.tif")
ETH <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/ETH_CHM_2020.tif")

#starting with pixel based results
nuts1 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/GBR_adm/GBR_adm1.shp")
#reprojecting into epsg2770
nuts1 <- project(nuts1, "epsg:27700")
#okay now doing this for TCD100m and CORINE, first need to aling to TCD100m
CORINE_alinged2 <-resample(CORINE, TCD10m, method = "near")
TCD100m <- resample(TCD100m, TCD10m, method = "near")
#okay getting a silvopasture raster
CORINE_pasture100m <- classify(CORINE_alinged2,
                               rcl = matrix(c(0, 17, 0,
                                     17, 18, 1,
                                     18, 256, 0), ncol = 3, byrow=TRUE
                                     ))

TCD100m5perc <- classify(TCD100m, rcl = matrix(c(0, 5, 0,
                                               4, 101, 1,
                                               254, 256, 0), 
                                             ncol = 3, byrow=TRUE
))

silvopasture100m <- CORINE_pasture100m+TCD100m5perc*CORINE_pasture100m
#calculating values for NUTs1
#note 10m by 10m pixels so dividing to get 100 km2
freq(crop(silvopasture100m, nuts1[1,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(silvopasture100m, nuts1[2,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(silvopasture100m, nuts1[3,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#SCOTLAND
freq(crop(silvopasture100m, nuts1[4,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(silvopasture100m, nuts1, mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#all


#for arable
CORINE_arable100m <- classify(CORINE_alinged2,
                               rcl = matrix(c(0, 11, 0,
                                              11, 14, 1,
                                              14, 18, 0,
                                              18, 22, 1,
                                              22, 256, 0), 
                                            ncol = 3, byrow=TRUE
                               ), include.lowest = FALSE)
silvoarable100m <- CORINE_arable100m+TCD100m5perc*CORINE_arable100m
#getting total
freq(crop(silvoarable100m, nuts1[1,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(silvoarable100m, nuts1[2,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(silvoarable100m, nuts1[3,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#SCOTLAND
freq(crop(silvoarable100m, nuts1[4,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(silvoarable100m, nuts1, mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#all

#all together
agroforestry100m <-CORINE_pasture100m+TCD100m5perc*CORINE_pasture100m+
  CORINE_arable100m + CORINE_arable100m*TCD100m5perc
freq(crop(agroforestry100m, nuts1[1,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(agroforestry100m, nuts1[2,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(agroforestry100m, nuts1[3,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#SCOTLAND
freq(crop(agroforestry100m, nuts1[4,], mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(agroforestry100m, nuts1, mask = TRUE,
          touches = FALSE),
     bylayer = FALSE)/(10000*100)#all




#for the 10mTCD
#CORINE_alinged2 <-resample(CORINE, TCD10m,method = 'near')
#making the rasters
TCD10m5perc <- classify(TCD10m, rcl = matrix(c(0, 5, 0,
                                                4, 101, 1,
                                                254, 256, 0), 
                                              ncol = 3, byrow=TRUE
))
CORINE_pasture10m <- classify(CORINE_alinged2,
                               rcl = matrix(c(0, 17, 0,
                                              17, 18, 1,
                                              18, 256, 0), ncol = 3, byrow=TRUE
                               ))
CORINE_arable10m <- classify(CORINE_alinged2,
                              rcl = matrix(c(0, 11, 0,
                                             11, 14, 1,
                                             14, 18, 0,
                                             18, 22, 1,
                                             22, 256, 0), 
                                           ncol = 3, byrow=TRUE
                              ), include.lowest = FALSE)
#silvopasture
CORINEsilvopasture10m <- CORINE_pasture10m+TCD10m5perc*CORINE_pasture10m
#note frequensies differ as the resolution is now 10m
freq(crop(CORINEsilvopasture10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(CORINEsilvopasture10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(CORINEsilvopasture10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(CORINEsilvopasture10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(CORINEsilvopasture10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all

#silvoarable
CORINEsilvoarable10m <- CORINE_arable10m+TCD10m5perc*CORINE_arable10m
freq(crop(CORINEsilvoarable10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(CORINEsilvoarable10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(CORINEsilvoarable10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(CORINEsilvoarable10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(CORINEsilvoarable10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all

#together
CORINEagroforestry10m <-CORINE_pasture10m+TCD10m5perc*CORINE_pasture10m+
  CORINE_arable10m + CORINE_arable10m*TCD10m5perc
freq(crop(CORINEagroforestry10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(CORINEagroforestry10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(CORINEagroforestry10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(CORINEagroforestry10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(CORINEagroforestry10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all


#doing this for OSMLU
ODSE <- resample(ODSE, TCD10m, method = 'near')
#making the maps
ODSE_pasture10m <- classify(ODSE,
                              rcl = matrix(c(0, 14, 0,
                                             14, 15, 1,
                                             15, 256, 0), ncol = 3, byrow=TRUE
                              ))
ODSE_arable10m <- classify(ODSE,
                              rcl = matrix(c(0, 8, 0,
                                             8, 11, 1,
                                             11, 256, 0), 
                                           ncol = 3, byrow=TRUE
                              ), include.lowest = FALSE)
#100m
ODSEsilvopasture100m <- ODSE_pasture10m+TCD100m5perc*ODSE_pasture10m
ODSEsilvoarable100m <- ODSE_arable10m+TCD100m5perc*ODSE_arable10m
ODSEagroforestry100m <-ODSE_pasture10m+TCD100m5perc*ODSE_pasture10m+
  ODSE_arable10m + ODSE_arable10m*TCD100m5perc
#10m
#silvopasture 100m
freq(crop(ODSEsilvopasture100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEsilvopasture100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEsilvopasture100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(ODSEsilvopasture100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEsilvopasture100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvopasture 10m
#silvoarable 100m
freq(crop(ODSEsilvoarable100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEsilvoarable100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEsilvoarable100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(ODSEsilvoarable100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEsilvoarable100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvoarable 10m
#agroforestry 100m
freq(crop(ODSEagroforestry100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEagroforestry100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEagroforestry100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(ODSEagroforestry100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEagroforestry100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all

#agroforestry 10m
ODSEsilvopasture10m <- ODSE_pasture10m+TCD10m5perc*ODSE_pasture10m
ODSEsilvoarable10m <- ODSE_arable10m+TCD10m5perc*ODSE_arable10m
ODSEagroforestry10m <-ODSE_pasture10m+TCD10m5perc*ODSE_pasture10m+
  ODSE_arable10m + ODSE_arable10m*TCD10m5perc
#silvopasture 10m
freq(crop(ODSEsilvopasture10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEsilvopasture10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEsilvopasture10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(ODSEsilvopasture10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEsilvopasture10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvoarable 100m
freq(crop(ODSEsilvoarable10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEsilvoarable10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEsilvoarable10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(ODSEsilvoarable10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEsilvoarable10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#agroforestry 10m
freq(crop(ODSEagroforestry10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(ODSEagroforestry10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(ODSEagroforestry10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(ODSEagroforestry10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(ODSEagroforestry10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all


#OSMLU
OSMLU <- resample(OSMLU, TCD10m, method = 'near')
#making the maps
OSM_pasture10m <- classify(OSMLU,
                            rcl = matrix(c(0, 22, 0,
                                           22, 23, 1,
                                           23, 256, 0), ncol = 3, byrow=TRUE
                            ))
OSM_arable10m <- classify(OSMLU,
                           rcl = matrix(c(0, 20, 0,
                                          20, 21, 1,
                                          21, 256, 0), 
                                        ncol = 3, byrow=TRUE
                           ), include.lowest = FALSE)
#100m
OSMsilvopasture100m <- OSM_pasture10m+TCD100m5perc*OSM_pasture10m
OSMsilvoarable100m <- OSM_arable10m+TCD100m5perc*OSM_arable10m
OSMagroforestry100m <-OSM_pasture10m+TCD100m5perc*OSM_pasture10m+
  OSM_arable10m + OSM_arable10m*TCD100m5perc
#silvopasture 100m
freq(crop(OSMsilvopasture100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMsilvopasture100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMsilvopasture100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(OSMsilvopasture100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMsilvopasture100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvopasture 10m
#silvoarable 100m
freq(crop(OSMsilvoarable100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMsilvoarable100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMsilvoarable100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(OSMsilvoarable100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMsilvoarable100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#agroforestry 100m
freq(crop(OSMagroforestry100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMagroforestry100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMagroforestry100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(OSMagroforestry100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMagroforestry100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#10m
OSMsilvopasture10m <- OSM_pasture10m+TCD10m5perc*OSM_pasture10m
OSMsilvoarable10m <- OSM_arable10m+TCD10m5perc*OSM_arable10m
OSMagroforestry10m <-OSM_pasture10m+TCD10m5perc*OSM_pasture10m+
  OSM_arable10m + OSM_arable10m*TCD10m5perc
#silvopasture 10m
freq(crop(OSMsilvopasture10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMsilvopasture10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMsilvopasture10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(OSMsilvopasture10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMsilvopasture10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvoarable 10m
freq(crop(OSMsilvoarable10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMsilvoarable10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMsilvoarable10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(OSMsilvoarable10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMsilvoarable10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#agroforestry 10m
freq(crop(OSMagroforestry10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(OSMagroforestry10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(OSMagroforestry10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(OSMagroforestry10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(OSMagroforestry10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all


#ukceh NOW
UKCEH <- resample(UKCEH, TCD10m, method = 'near')
UKCEH_pasture10m <- classify(UKCEH,
                           rcl = matrix(c(0, 3, 0,
                                          3, 4, 1,
                                          4, 256, 0), ncol = 3, byrow=TRUE
                           ))
UKCEH_arable10m <- classify(UKCEH,
                          rcl = matrix(c(0, 2, 0,
                                         2, 3, 1,
                                         3, 256, 0), 
                                       ncol = 3, byrow=TRUE
                          ), include.lowest = FALSE)
#100m
UKCEHsilvopasture100m <- UKCEH_pasture10m+TCD100m5perc*UKCEH_pasture10m
UKCEHsilvoarable100m <- UKCEH_arable10m+TCD100m5perc*UKCEH_arable10m
UKCEHagroforestry100m <-UKCEH_pasture10m+TCD100m5perc*UKCEH_pasture10m+
  UKCEH_arable10m + UKCEH_arable10m*TCD100m5perc

#silvopasture 100m
freq(crop(UKCEHsilvopasture100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHsilvopasture100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHsilvopasture100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(UKCEHsilvopasture100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHsilvopasture100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvopasture 10m
#silvoarable 100m
freq(crop(UKCEHsilvoarable100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHsilvoarable100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHsilvoarable100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(UKCEHsilvoarable100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHsilvoarable100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#agroforestry 100m
freq(crop(UKCEHagroforestry100m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHagroforestry100m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHagroforestry100m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(UKCEHagroforestry100m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHagroforestry100m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#10m
UKCEHsilvopasture10m <- UKCEH_pasture10m+TCD10m5perc*UKCEH_pasture10m
UKCEHsilvoarable10m <- UKCEH_arable10m+TCD10m5perc*UKCEH_arable10m
UKCEHagroforestry10m <-UKCEH_pasture10m+TCD10m5perc*UKCEH_pasture10m+
  UKCEH_arable10m + UKCEH_arable10m*TCD10m5perc
#silvopasture 10m
freq(crop(UKCEHsilvopasture10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHsilvopasture10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHsilvopasture10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#Scotland
freq(crop(UKCEHsilvopasture10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHsilvopasture10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#silvoarable 10m
freq(crop(UKCEHsilvoarable10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHsilvoarable10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHsilvoarable10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(UKCEHsilvoarable10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHsilvoarable10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all
#agroforestry 10m
freq(crop(UKCEHagroforestry10m, nuts1[1,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#england
freq(crop(UKCEHagroforestry10m, nuts1[2,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#NI
freq(crop(UKCEHagroforestry10m, nuts1[3,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#scotland
freq(crop(UKCEHagroforestry10m, nuts1[4,], mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#wales
freq(crop(UKCEHagroforestry10m, nuts1, mask = TRUE, touches = FALSE),
     bylayer = FALSE)/(10000*100)#all


#now for analysing the 
library(dplyr)

NI <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/country_wide/NI_results.csv")
#UKCEH
#TCD100m
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
  group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#TCD10m
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#SWF
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$UKCEH %in% c(3,4)),] %>% summarise(Class = UKCEH, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full

#CORINE
#TCD100m
View(NI[which(NI$CORINE %in% c(18)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$CORINE %in% c(12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$CORINE %in% c(18,12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#TCD10m
View(NI[which(NI$CORINE %in% c(18)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$CORINE %in% c(12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$CORINE %in% c(18,12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#SWF
View(NI[which(NI$CORINE %in% c(18)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$CORINE %in% c(12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$CORINE %in% c(18,12,13,14,19,20,21,22)),] %>% summarise(Class = CORINE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full

#ODSE
#TCD100m
View(NI[which(NI$ODSE %in% c(15)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$ODSE %in% c(9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$ODSE %in% c(15,9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#TCD10m
View(NI[which(NI$ODSE %in% c(15)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$ODSE %in% c(9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$ODSE %in% c(15,9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#SWF
View(NI[which(NI$ODSE %in% c(15)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #pasture
View(NI[which(NI$ODSE %in% c(9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #arable
View(NI[which(NI$ODSE %in% c(15,9,10,11)),] %>% summarise(Class = ODSE, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full

#OSMLU
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(TCD100m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#TCD10m
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(TCD10m > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full
#SWF
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(Class, trees) %>% summarise(A = sum(Area)/10^8))
View(NI[which(NI$OSM %in% c(23,21)),] %>% summarise(Class = OSM, Area = area, trees = as.numeric(SWF > 5)) %>% 
       group_by(trees) %>% summarise(A = sum(Area)/10^8)) #for full


for(i in 1001:2000){
  results$area[i] <- expanse(NI_parcels[i,])
  #buffer
  area <- NI_parcels[i,]
  if(length(area) != 0){
    #landcovs
    n3 <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
    if(length(n3 != 0)){
      results$CORINE[i] <- n3
    }
    n2 <-  names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
    if(length(n2 != 0)){
      results$ODSE[i] <-n2
    }
    n <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
    if(length(n) != 0){
      results$OSM[i] <- n
    }
    #results$OSM[i] <- names(sort(-table(extract(OSMLU, NI_parcels[i,], ID = FALSE))))[1]
    if(results$CORINE[i] %in% c(12,13,14,18,19,20,21,22) |
       results$ODSE[i] %in% c(9,10,11,15) |
       results$OSM[i] %in% c(21,23) |
       NI_parcels$'_mode'[i] %in% c(3,4)) {
      area2 <- buffer(area, width = -5)
      if(length(area2) != 0){
      
      results$TCD10m[i] <- zonal(TCD10m, area2, weights = TRUE)
      results$TCD100m[i] <- zonal(TCD100m, area2, weights = TRUE)
      results$ETH_CHM[i] <- (sum(as.numeric(extract(ETH, area2, ID = FALSE)>3))/length(unlist(
        extract(ETH, area2, ID = FALSE))))*100
      
      #vector trees
      trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                        extent = ext(area2))
      results$SWF[i] <- (sum(expanse(intersect(trees_SWF, area2)))/expanse(area2))*100
      }
    }}
  
}

saveRDS(results, "prelim_results_newbuffer.rds")
