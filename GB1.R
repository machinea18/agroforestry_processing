#install.packages("terra")
library(terra)
library(parallel)
library(lme4)
NI_parcels <- vect("/data/clcr/am1355/agroforestry_processing/OSMM_processed/GB1.gpkg")

results <- data.frame(CORINE = rep(NA, length(NI_parcels)), ODSE = NA,
                      OSM = NA, UKCEH = NA, TCD10m = NA, TCD100m = NA,
                      SWF = NA, ETH_CHM = NA, TOW = NA,area = NA)

results2 <- results

#reading
#results <- readRDS("prelim_results2.rds")
#rasters
CORINE <- rast("/data/clcr/am1355/agroforestry_processing/CORINE_2018.tif")
ODSE <- rast("/data/clcr/am1355/agroforestry_processing/ODSE_2020.tif")
OSMLU <- rast("/data/clcr/am1355/agroforestry_processing/OSMLU.tif")
UKCEH <- rast("/data/clcr/am1355/agroforestry_processing/UKCEH.tif")
#trees
TCD10m <- rast("/data/clcr/am1355/agroforestry_processing/tree10m2.tif")
TCD100m <- rast("/data/clcr/am1355/agroforestry_processing/tree100m2.tif")
#ETH <- rast("/data/clcr/am1355/agroforestry_processing/ETH_CHM_2020.tif")

#writing the processing function
parcel_processing <- function(results_table, parcels, x) {
  results_table$area[x] <- parcels$calculated[x]
  area <- parcels[x,]
  #landcovers
  n1 <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
  if(length(n1) != 0) {results_table$CORINE[x] <- n1}
  #ODSE
  n2 <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
  if(length(n2) != 0) {results_table$ODSE[x] <- n2}
  #OSMLU
  n3 <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
  if(length(n3) != 0) {results_table$OSM[x] <- n3}
#UKCEH
  n4 <- names(sort(-table(extract(UKCEH, area, ID = FALSE))))[1]
  if(length(n4) != 0) {results_table$UKCEH[x] <- n4}

	#now for all the treecov stuff
  #only do it if necessary
  if(results_table$CORINE[x] %in% c(12,13,14,18,19,20,21,22) |
     results_table$ODSE[x] %in% c(9,10,11,15) |
     results_table$OSM[x] %in% c(21,23) |
     results_table$UKCEH[x] %in% c(3,4)) {
    area2 <- buffer(area, width = -5) 
    if(length(area2) != 0) {
      results_table$TCD10m[x] <- zonal(TCD10m, area2, weights = TRUE)
      results_table$TCD100m[x] <- zonal(TCD100m, area2, weights = TRUE)
      #results_table$ETH_CHM[x] <- (sum(as.numeric(extract(ETH, area2, ID = FALSE)>3))/length(unlist(
        #extract(ETH, area2, ID = FALSE))))*100
      #SWF
      trees_SWF <- vect("/data/clcr/am1355/agroforestry_processing/SWF.gpkg",
                        extent = ext(area2))
      results_table$SWF[x] <- (sum(expanse(intersect(trees_SWF, area2)))/expanse(area2))*100
#TOW (note only valid for england!
      trees_TOW <- vect("/data/clcr/am1355/agroforestry_processing/TOW_all.gpkg",
                        extent = ext(area2))
     results_table$TOW[x] <- (sum(expanse(intersect(trees_TOW, area2)))/expanse(area2))*100
    }

  }
  
  return(results_table[x,])
  
}

#nums (how many I want to do)
print(length(NI_parcels))
starts <- 1
nums <- 100000
first_results <- mclapply( X = starts:nums, FUN = parcel_processing, mc.cores =20, results_table = results[starts:nums,],
         parcels = NI_parcels[starts:nums,])



first_results <- matrix(unlist(first_results), byrow = TRUE,
                             nrow = nums, 
                             ncol = ncol(results)) #getting matrix of results
colnames(first_results) <- names(results)

write.csv(first_results, "/data/clcr/am1355/agroforestry_processing/GB1_results2.csv")
