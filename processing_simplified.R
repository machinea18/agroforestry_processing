library(terra)
points_all2 <- vect("study_sites/UK_points.gpkg")
#rasters
CORINE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/CORINE_2018.tif")
ODSE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/ODSE_2020.tif")
OSMLU <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc//OSMLU/OSMLU.tif")
#trees
TCD10m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree10m2.tif")
TCD100m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree100m2.tif")
ETH <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/ETH_CHM_2020.tif")

names <- unique(points_all2$Name)
names_tif <- c("Glensaugh", "Henfaes", "Parkhill", "CarbonNeut", "Dartington", 
               "FarmED", "Coombe", "Shropshire", "ThreeHagges", "Tolhurst", 
               "wakelyns", "WesEmlett", "Whitehall", "Loughgall")


#temp again (UGH)
pall_non <- vect("study_sites/pall_processed_THESIS.gpkg")
pall_1m <- vect("study_sites/pall_processed_THESIS_buffer1m.gpkg")
pall_2m <- vect("study_sites/pall_processed_THESIS_buffer2m.gpkg")
pall_3m <- vect("study_sites/pall_processed_THESIS_buffer3m.gpkg")
pall_4m <- vect("study_sites/pall_processed_THESIS_buffer4m.gpkg")
for(i in 1:length(pall_non)) {
  if(pall_non$Name[i] == "Loughgall") {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/BNG.gpkg",
                 extent = ext(pall_non[i,]))
  } else {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
                 extent = ext(pall_non[i,]))
  }
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  pall_non$UKCEH_SWF <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #1m
  area <- buffer(area, width = -1)
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  pall_1m$UKCEH_SWF <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #2m
  area <- buffer(area, width = -1)
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  pall_2m$UKCEH_SWF <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #3m
  area <- buffer(area, width = -1)
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  pall_3m$UKCEH_SWF <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #4m
  area <- buffer(area, width = -1)
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  pall_4m$UKCEH_SWF <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #for OSM
  #OSM
  area2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/OSMM_studysites/OSM_studysites_GB.gpkg",
                extent = ext(pall_non[i,])) 
  #area2 <- buffer(area2, width = -4) 
  if(length(area2) != 0) {
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    pall_non$OSM_SWF <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
    #1m
    area2 <- buffer(area2, width = -1)
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    pall_1m$OSM_SWF <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
    #2m
    area2 <- buffer(area2, width = -1)
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    pall_2m$OSM_SWF <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
    #3m
    area2 <- buffer(area2, width = -1)
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    pall_3m$OSM_SWF <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
    #4m
    area2 <- buffer(area2, width = -1)
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    pall_4m$OSM_SWF <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
  }
  
}

writeVector(pall_non, "study_sites/pall_processed_THESIS.gpkg", overwrite = TRUE)
writeVector(pall_1m, "study_sites/pall_processed_THESIS_buffer1m.gpkg", overwrite = TRUE)
writeVector(pall_2m, "study_sites/pall_processed_THESIS_buffer2m.gpkg", overwrite = TRUE)
writeVector(pall_3m, "study_sites/pall_processed_THESIS_buffer3m.gpkg", overwrite = TRUE)
writeVector(pall_4m, "study_sites/pall_processed_THESIS_buffer4m.gpkg", overwrite = TRUE)

for(i in 1:length(points_all2)) {
  if(points_all2$Name[i] == "Loughgall") {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/BNG.gpkg",
                 extent = ext(points_all2[i,]))
    area <- buffer(area, width = -5) #neg buffer removed
  } else {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
                 extent = ext(points_all2[i,]))
    area <- buffer(area, width = -5) 
    
  }
  meta_trees <- rast(paste("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/Meta_CHM/",
                     names_tif[which(names %in% points_all2$Name[i])],
                     ".tif", sep = ""))
  #pixel
  points_all2$CORINE[i] <- extract(CORINE, points_all2[i,], ID =FALSE)
  points_all2$ODSE[i] <- extract(ODSE, points_all2[i,], ID = FALSE)
  points_all2$OSMLU[i] <- extract(OSMLU, points_all2[i,], ID = FALSE)
  points_all2$TCD10m[i] <- extract(TCD10m, points_all2[i,], ID = FALSE)
  points_all2$TCD100m[i] <- extract(TCD100m, points_all2[i,], ID = FALSE)
  points_all2$ETH[i] <- as.numeric(extract(ETH, points_all2[i,], ID = FALSE)>0)
  points_all2$Meta_CHM[i] <- as.numeric(extract(meta_trees, points_all2[i,], ID = FALSE)>0)
  
  #UKCEH
  points_all2$UKCEH[i] <- area$`_mode`
  points_all2$UKCEH_CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
  points_all2$UKCEH_ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
  points_all2$UKCEH_OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
  points_all2$UKCEH_TCD10m[i] <- zonal(TCD10m, area, weights = TRUE)
  points_all2$UKCEH_TCD100m[i] <- zonal(TCD100m, area, weights = TRUE)
  points_all2$UKCEH_ETH[i] <- (sum(as.numeric(extract(ETH, area, ID = FALSE)>0))/length(unlist(
    extract(ETH, area, ID = FALSE))))*100
  points_all2$UKCEH_meta_CHM[i] <- (sum(as.numeric(extract(meta_trees, area, ID = FALSE)>0))/length(unlist(
    extract(meta_trees, area, ID = FALSE))))*100
  trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                extent = ext(area))
  trees_bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/NTM.gpkg",
                        extent = ext(area))
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                    extent = ext(area))
  trees_deepforest_25cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_25cm.gpkg",
                                extent = ext(area))
  
  #getting the percentage treecover
  points_all2$UKCEH_TOW[i] <- (sum(expanse(intersect(trees, area)))/expanse(area))*100
  points_all2$UKCEH_bluesky[i] <- (sum(expanse(intersect(trees_bluesky, area)))/expanse(area))*100
  points_all2$UKCEH_SWF[i] <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #deepforest
  points_all2$UKCEH_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm, area)))/expanse(area))*100
  
  #OSM
  area2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/OSMM_studysites/OSM_studysites_GB.gpkg",
                extent = ext(points_all2[i,])) 
  area2 <- buffer(area2, width = -5) 
  if(length(area2) != 0) {
    points_all2$OSM_CORINE[i] <- names(sort(-table(extract(CORINE, area2, ID = FALSE))))[1]
    points_all2$OSM_ODSE[i] <- names(sort(-table(extract(ODSE, area2, ID = FALSE))))[1]
    points_all2$OSM_OSMLU[i] <- names(sort(-table(extract(OSMLU, area2, ID = FALSE))))[1]
    points_all2$OSM_TCD10m[i] <- zonal(TCD10m, area2, weights = TRUE)
    points_all2$OSM_TCD100m[i] <- zonal(TCD100m, area2, weights = TRUE) 
    points_all2$OSM_ETH[i] <- (sum(as.numeric(extract(ETH, area2, ID = FALSE)>0))/length(unlist(
      extract(ETH, area2, ID = FALSE))))*100
    points_all2$OSM_meta_CHM[i] <- (sum(as.numeric(extract(meta_trees, area2, ID = FALSE)>0))/length(unlist(
      extract(meta_trees, area2, ID = FALSE))))*100
    #trees
    trees2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                   extent = ext(area2))
    trees_bluesky2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/NTM.gpkg",
                           extent = ext(area2))
    trees_SWF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                       extent = ext(area2))
    trees_deepforest_25cm2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_25cm.gpkg",
                                   extent = ext(area2))
    points_all2$OSM_TOW[i] <- (sum(expanse(intersect(trees2, area2)))/expanse(area2))*100
    points_all2$OSM_bluesky[i] <- (sum(expanse(intersect(trees_bluesky2, area2)))/expanse(area2))*100
    points_all2$OSM_SWF[i] <- (sum(expanse(intersect(trees_SWF2, area2)))/expanse(area2))*100
    #deepforest
    points_all2$OSM_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm2, area2)))/expanse(area2))*100
  }else {
    points_all2$OSM_CORINE[i] <- NA
    points_all2$OSM_ODSE[i] <- NA
    points_all2$OSM_OSMLU[i] <- NA
    points_all2$OSM_TCD10m[i] <- NA
    points_all2$OSM_TCD100m[i] <- NA
    points_all2$OSM_TOW[i] <- NA
    points_all2$OSM_bluesky[i] <- NA
    points_all2$OSM_SWF[i] <- NA
    points_all2$OSM_DF25[i] <- NA
    points_all2$OSM_ETH[i] <- NA
    points_all2$OSM_meta_CHM[i] <- NA
  }
  
}




writeVector(points_all2, "study_sites/pall_processed_THESIS_buffer5m.gpkg",
            overwrite = TRUE)
#naming
UKCEH_labs <- c("Woodland", "Rock", "Woodland", "Suburban", 
                "Arable", "Pasture", "Grassland")
UKCEH_cols <- c("#0C6805", "#D3D0FD", "#828282", "#642722",
                "#e6e64d", "#09FA0B")
CORINE_cols <- c("#ffe6ff", "#642722", "#e6e64d", "#0C6805",
                 "#09FA0B", "#6ea3d5", "#80f2e6", "black")
#
#classing
#getting non mature names
nm_names <- c("Wes Emlett", "Tolhurst Organics", "Parkhill", "Hill and Coombe Farm",
              "Dartington Trust", "Carbon Neutral Beef")
#now checking the histograms
library(ggplot2)
pall_dat <- as.data.frame(points_all2)
ppall_dat <- pall_dat[which(!(pall_dat$Name %in% nm_names)),]
pE_dat <- pall_dat[which(pall_dat$Name != "Parkhill" &
                            pall_dat$Name != "Glensaugh" &
                            pall_dat$Name != "Loughgall" &
                           pall_dat$Name != "Henfaes"),]
pGB_dat <- pall_dat[which(pall_dat$Name != "Loughgall"),]
ggplot(data=pall_dat, aes(y = TCD10m, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pall_dat, aes(y = TCD100m, x = Type)) + geom_violin()+geom_hline(yintercept =5)

#UKCEH
ggplot(data=pall_dat, aes(y = UKCEH_TCD10m, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pall_dat, aes(y = UKCEH_TCD100m, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pall_dat, aes(y = UKCEH_SWF, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pall_dat, aes(y = UKCEH_DF25, x = Type)) + geom_violin() +geom_hline(yintercept =5)
#GBUKCEH
ggplot(data=pGB_dat, aes(y = UKCEH_bluesky, x = Type)) + geom_violin()+geom_hline(yintercept =5)
#OSM
ggplot(data=pGB_dat, aes(y = OSM_TCD10m, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pGB_dat, aes(y = OSM_TCD100m, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pGB_dat, aes(y = OSM_SWF, x = Type)) + geom_violin()+geom_hline(yintercept =5)
ggplot(data=pGB_dat, aes(y = OSM_DF25, x = Type)) + geom_violin() +geom_hline(yintercept =5)
ggplot(data=pGB_dat, aes(y = OSM_bluesky, x = Type)) + geom_violin()+geom_hline(yintercept =5)
#TOW
ggplot(data=pE_dat, aes(y = OSM_TOW, x = Type)) + geom_boxplot() +geom_hline(yintercept =5)
ggplot(data=pE_dat, aes(y = UKCEH_TOW, x = Type)) + geom_violin()+geom_hline(yintercept =5)

#renaming buffered labels
#points_all2 <- vect("study_sites/pall_processed_THESIS_buffer.gpkg")
points_all2 <- vect("study_sites/pall_processed_THESIS.gpkg")
#buffer helps loads
#bluesky outperforms without the buffer
#worth looking into what buffer is best...

#adding labels
points_all2$UKCEH <- factor(points_all2$UKCEH, levels = c(1, 12, 2, 21, 3, 4, 5),
                           labels = c("Woodland", "Rock", "Woodland", "Suburban", 
                                      "Arable", "Pasture", "Grassland"))
#CORINE
points_all2$CORINE <- factor(points_all2$CORINE, 
                                   labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                              "Arable", "Trees", "Trees", "Trees", "Grass",
                                              "Moores", "Water"),
                                   levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
points_all2$UKCEH_CORINE <- factor(points_all2$UKCEH_CORINE, 
                                  labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                             "Arable", "Trees", "Trees", "Trees", "Grass",
                                             "Moores", "Water"),
                                  levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
points_all2$OSM_CORINE <- factor(points_all2$OSM_CORINE, 
                                   labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                              "Arable", "Trees", "Trees", "Trees", "Grass",
                                              "Moores", "Water"),
                                   levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
#ODSE
points_all2$ODSE <- factor(points_all2$ODSE, 
                             levels = c(1,8,9,15,16,17,18,19,21,28),
                             labels = c("Urban", "Arable", "Arable",
                                        "Pasture", "Trees", "Trees", "Grass",
                                        "Moores", "Trees", "Snow"))
points_all2$UKCEH_ODSE <- factor(points_all2$UKCEH_ODSE, 
                           levels = c(1,8,9,15,16,17,18,19,21,28),
                           labels = c("Urban", "Arable", "Arable",
                                      "Pasture", "Trees", "Trees", "Grass",
                                      "Moores", "Trees", "Snow"))
points_all2$OSM_ODSE <- factor(points_all2$OSM_ODSE, 
                           levels = c(1,8,9,15,16,17,18,19,21,28),
                           labels = c("Urban", "Arable", "Arable",
                                      "Pasture", "Trees", "Trees", "Grass",
                                      "Moores", "Trees", "Snow"))
#OSMLU
points_all2$OSMLU <- factor(points_all2$OSMLU, 
                           levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                           labels = c("Water", "Urban", "Industrial", "Mines",
                                      "Vegetation", "Arable", "Orchards", 
                                      "Pasture", "Trees", "Vegetation", "Bare",
                                      "Wetlands"))
points_all2$UKCEH_OSMLU <- factor(points_all2$UKCEH_OSMLU, 
                            levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                            labels = c("Water", "Urban", "Industrial", "Mines",
                                       "Vegetation", "Arable", "Orchards", 
                                       "Pasture", "Trees", "Vegetation", "Bare",
                                       "Wetlands"))
points_all2$OSM_OSMLU <- factor(points_all2$OSM_OSMLU, 
                            levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                            labels = c("Water", "Urban", "Industrial", "Mines",
                                       "Vegetation", "Arable", "Orchards", 
                                       "Pasture", "Trees", "Vegetation", "Bare",
                                       "Wetlands"))
#classing
points_all_results <- data.frame(Parcel=rep(NA,6*3*3),Landcov = rep(NA, 6*3*3), 
                                 Treecov=NA, SA =NA,SA_mat= NA, SP = NA, 
                                 SP_mat = NA, AC = NA, WC =NA)
points_all_results$Parcel <- c(rep(NA,3*6), rep("UKCEH", 3*6),
                               rep("OSM", 3*6))
points_all_results$Landcov <- rep(c(rep("CORINE", 6),
                                rep("ODSE", 6), rep("OSMLU", 6)),3)
points_all_results$Treecov <- rep(c("TCD10m", "TCD100m", "SWF", "DF25",
                                    "Bluesky", "TOW"), 3)
lands <- c(rep(9,6),rep(10,6),rep(11,6),
           rep(15,6),rep(16,6),rep(17,6),
           rep(24,6),rep(25,6),rep(26,6)) #col numbers for landcovers, in oder
pall_dat <- as.data.frame(points_all2)
pGB_dat <-pall_dat[which(pall_dat$Name != "Loughgall"),]
pE_dat <-pall_dat[which(pall_dat$Name != "Loughgall" &
                          pall_dat$Name != "Glensaugh" &
                          pall_dat$Name != "Parkhill" &
                          pall_dat$Name != "Henfaes"),]
trees <- c(rep(c(12,13,12,13,12,13),3), rep(c(18,19,22,23,21,20),3),
           rep(c(27,28,31,32,30,33),3))
for(i in 1:length(trees)){
  if(points_all_results$Treecov[i] == "Bluesky") {
    #SA
    points_all_results[i,4] <- length(which((pGB_dat[which(pGB_dat$Type == "SA"),lands[i]] ==
                                               "Pasture" | pGB_dat[which(pGB_dat$Type == "SA"),lands[i]] ==
                                               "Arable") & (pGB_dat[which(pGB_dat$Type == "SA"),trees[i]] >5 &
                                                              pGB_dat[which(pGB_dat$Type == "SA"),trees[i]] <100)))/length(which(pGB_dat$Type == "SA"))*100
    #SAmat
    points_all_results[i,5] <- length(which((pGB_dat[which(pGB_dat$Type == "SA" & pGB_dat$Planted < 2000),lands[i]] ==
                                               "Pasture" | pGB_dat[which(pGB_dat$Type == "SA"
                                                                          & pGB_dat$Planted < 2000),lands[i]] ==
                                               "Arable") & (pGB_dat[which(pGB_dat$Type == "SA"
                                                                           & pGB_dat$Planted < 2000),trees[i]] >5 &
                                                              pGB_dat[which(pGB_dat$Type == "SA"
                                                                             & pGB_dat$Planted < 2000),trees[i]] <100)))/length(which(pGB_dat$Type == "SA"
                                                                                                                                          & pGB_dat$Planted < 2000))*100
    #SP
    points_all_results[i,6] <- length(which((pGB_dat[which(pGB_dat$Type == "SP"),lands[i]] ==
                                               "Pasture" | pGB_dat[which(pGB_dat$Type == "SP"),lands[i]] ==
                                               "Arable") & (pGB_dat[which(pGB_dat$Type == "SP"),trees[i]] >5 &
                                                              pGB_dat[which(pGB_dat$Type == "SP"),trees[i]] <100)))/length(which(pGB_dat$Type == "SP"))*100
    #SPmat
    points_all_results[i,7] <- length(which((pGB_dat[which(pGB_dat$Type == "SP" & pGB_dat$Planted < 2000),lands[i]] ==
                                               "Pasture" | pGB_dat[which(pGB_dat$Type == "SP"
                                                                          & pGB_dat$Planted < 2000),lands[i]] ==
                                               "Arable") & (pGB_dat[which(pGB_dat$Type == "SP"
                                                                           & pGB_dat$Planted < 2000),trees[i]] >5 &
                                                              pGB_dat[which(pGB_dat$Type == "SP"
                                                                             & pGB_dat$Planted < 2000),trees[i]] <100)))/length(which(pGB_dat$Type == "SP"
                                                                                                                                          & pGB_dat$Planted < 2000))*100
    #AC
    points_all_results[i,8] <- length(which((pGB_dat[which(pGB_dat$Type == "AC"),lands[i]] !=
                                               "Pasture" & pGB_dat[which(pGB_dat$Type == "AC"),lands[i]] !=
                                               "Arable") |
                                              pGB_dat[which(pGB_dat$Type == "AC"),trees[i]] <5 |
                                              pGB_dat[which(pGB_dat$Type == "AC"),trees[i]] >100))/length(which(pGB_dat$Type == "AC"))*100
    #WC
    points_all_results[i,9] <- length(which((pGB_dat[which(pGB_dat$Type == "WC"),lands[i]] !=
                                               "Pasture" & pGB_dat[which(pGB_dat$Type == "WC"),lands[i]] !=
                                               "Arable") |
                                              pGB_dat[which(pGB_dat$Type == "WC"),trees[i]] <5 |
                                              pGB_dat[which(pGB_dat$Type == "WC"),trees[i]] >100))/length(which(pGB_dat$Type == "WC"))*100
    
  } else {
    if(points_all_results$Treecov[i] == "TOW") {
      
      #SA
      points_all_results[i,4] <- length(which((pE_dat[which(pE_dat$Type == "SA"),lands[i]] ==
                                                 "Pasture" | pE_dat[which(pE_dat$Type == "SA"),lands[i]] ==
                                                 "Arable") & (pE_dat[which(pE_dat$Type == "SA"),trees[i]] >5 &
                                                                pE_dat[which(pE_dat$Type == "SA"),trees[i]] <100)))/length(which(pE_dat$Type == "SA"))*100
      #SAmat
      points_all_results[i,5] <- length(which((pE_dat[which(pE_dat$Type == "SA" & pE_dat$Planted < 2000),lands[i]] ==
                                                 "Pasture" | pE_dat[which(pE_dat$Type == "SA"
                                                                           & pE_dat$Planted < 2000),lands[i]] ==
                                                 "Arable") & (pE_dat[which(pE_dat$Type == "SA"
                                                                            & pE_dat$Planted < 2000),trees[i]] >5 &
                                                                pE_dat[which(pE_dat$Type == "SA"
                                                                              & pE_dat$Planted < 2000),trees[i]] <100)))/length(which(pE_dat$Type == "SA"
                                                                                                                                       & pE_dat$Planted < 2000))*100
      #SP
      points_all_results[i,6] <- length(which((pE_dat[which(pE_dat$Type == "SP"),lands[i]] ==
                                                 "Pasture" | pE_dat[which(pE_dat$Type == "SP"),lands[i]] ==
                                                 "Arable") & (pE_dat[which(pE_dat$Type == "SP"),trees[i]] >5 &
                                                                pE_dat[which(pE_dat$Type == "SP"),trees[i]] <100)))/length(which(pE_dat$Type == "SP"))*100
      #SPmat
      points_all_results[i,7] <- length(which((pE_dat[which(pE_dat$Type == "SP" & pE_dat$Planted < 2000),lands[i]] ==
                                                 "Pasture" | pE_dat[which(pE_dat$Type == "SP"
                                                                           & pE_dat$Planted < 2000),lands[i]] ==
                                                 "Arable") & (pE_dat[which(pE_dat$Type == "SP"
                                                                            & pE_dat$Planted < 2000),trees[i]] >5 &
                                                                pE_dat[which(pE_dat$Type == "SP"
                                                                              & pE_dat$Planted < 2000),trees[i]] <100)))/length(which(pE_dat$Type == "SP"
                                                                                                                                       & pE_dat$Planted < 2000))*100
      #AC
      points_all_results[i,8] <- length(which((pE_dat[which(pE_dat$Type == "AC"),lands[i]] !=
                                                 "Pasture" & pE_dat[which(pE_dat$Type == "AC"),lands[i]] !=
                                                 "Arable") |
                                                pE_dat[which(pE_dat$Type == "AC"),trees[i]] <5 |
                                                pE_dat[which(pE_dat$Type == "AC"),trees[i]] >100))/length(which(pE_dat$Type == "AC"))*100
      #WC
      points_all_results[i,9] <- length(which((pE_dat[which(pE_dat$Type == "WC"),lands[i]] !=
                                                 "Pasture" & pE_dat[which(pE_dat$Type == "WC"),lands[i]] !=
                                                 "Arable") |
                                                pE_dat[which(pE_dat$Type == "WC"),trees[i]] <5 |
                                                pE_dat[which(pE_dat$Type == "WC"),trees[i]] >100))/length(which(pE_dat$Type == "WC"))*100
      
      
      } else {
        #SA
        points_all_results[i,4] <- length(which((pall_dat[which(points_all2$Type == "SA"),lands[i]] ==
                                                               "Pasture" | pall_dat[which(points_all2$Type == "SA"),lands[i]] ==
                                                               "Arable") & (pall_dat[which(points_all2$Type == "SA"),trees[i]] >5 &
                                                                              pall_dat[which(points_all2$Type == "SA"),trees[i]] <100)))/length(which(points_all2$Type == "SA"))*100
        #SAmat
        points_all_results[i,5] <- length(which((pall_dat[which(points_all2$Type == "SA" & points_all2$Planted < 2000),lands[i]] ==
                                                             "Pasture" | pall_dat[which(points_all2$Type == "SA"
                                                                                        & points_all2$Planted < 2000),lands[i]] ==
                                                             "Arable") & (pall_dat[which(points_all2$Type == "SA"
                                                                                         & points_all2$Planted < 2000),trees[i]] >5 &
                                                                            pall_dat[which(points_all2$Type == "SA"
                                                                                           & points_all2$Planted < 2000),trees[i]] <100)))/length(which(points_all2$Type == "SA"
                                                                                                                                                      & points_all2$Planted < 2000))*100
        #SP
        points_all_results[i,6] <- length(which((pall_dat[which(points_all2$Type == "SP"),lands[i]] ==
                                                   "Pasture" | pall_dat[which(points_all2$Type == "SP"),lands[i]] ==
                                                   "Arable") & (pall_dat[which(points_all2$Type == "SP"),trees[i]] >5 &
                                                                  pall_dat[which(points_all2$Type == "SP"),trees[i]] <100)))/length(which(points_all2$Type == "SP"))*100
        #SPmat
        points_all_results[i,7] <- length(which((pall_dat[which(points_all2$Type == "SP" & points_all2$Planted < 2000),lands[i]] ==
                                                   "Pasture" | pall_dat[which(points_all2$Type == "SP"
                                                                              & points_all2$Planted < 2000),lands[i]] ==
                                                   "Arable") & (pall_dat[which(points_all2$Type == "SP"
                                                                               & points_all2$Planted < 2000),trees[i]] >5 &
                                                                  pall_dat[which(points_all2$Type == "SP"
                                                                                 & points_all2$Planted < 2000),trees[i]] <100)))/length(which(points_all2$Type == "SP"
                                                                                                                                             & points_all2$Planted < 2000))*100
        #AC
        points_all_results[i,8] <- length(which((pall_dat[which(points_all2$Type == "AC"),lands[i]] !=
                                                             "Pasture" & pall_dat[which(points_all2$Type == "AC"),lands[i]] !=
                                                             "Arable") |
                                                            pall_dat[which(points_all2$Type == "AC"),trees[i]] <5 |
                                                            pall_dat[which(points_all2$Type == "AC"),trees[i]] >100))/length(which(points_all2$Type == "AC"))*100
        #WC
        points_all_results[i,9] <- length(which((pall_dat[which(points_all2$Type == "WC"),lands[i]] !=
                                                   "Pasture" & pall_dat[which(points_all2$Type == "WC"),lands[i]] !=
                                                   "Arable") |
                                                  pall_dat[which(points_all2$Type == "WC"),trees[i]] <5 |
                                                  pall_dat[which(points_all2$Type == "WC"),trees[i]] >100))/length(which(points_all2$Type == "WC"))*100
      
    }
  }
}

#treecover alone isn't enough to distinguish these...

