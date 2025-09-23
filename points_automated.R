library(terra)
points <- vect("study_sites/england_points.gpkg")
points_GB <- vect("study_sites/GB_points.gpkg")
points_all <- vect("study_sites/UK_points.gpkg")
woodpasture <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/woodpasture_defra/wood_pasture_and_parkland.gpkg")

#getting only silvopasture
n <- which(points$Name %in% c("Carbon Neutral Beef", "Dartington Trust", "FarmED",
                              "Hill and Coombe Farm", "Shropshire Agroforestry Project",
                              "Three Hagges Woodmeadow", "Wes Emlett"))
silvopasture <- points[n,]
#getting landcover maps
CORINE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/CORINE_2018.tif")
ODSE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/ODSE_2020.tif")
OSMLU <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc//OSMLU/UK_10m_3035_tiled.tif")
#trees
TCD10m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree10m2.tif")
TCD100m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree100m2.tif")

#DEFRA woodpasture
defra <- extract(woodpasture, silvopasture)
#checking that the silvopasture is included
length(which(!is.na(defra$objectid[which(silvopasture$Type == "SP")])))/ length(which(silvopasture$Type== "SP"))
#0% accuracy for correctly identifying silvopasture
length(which(is.na(defra$objectid[which(silvopasture$Type != "SP")])))/ length(which(silvopasture$Type!= "SP"))
#100% accurate in not counting the
#points <- points_GB

points$classes_UKCEH <- NA
points$classes_UKCEH_TOW <- NA
points$classes_UKCEH_deepforest <- rep(NA, length(points))
points$classes_UKCEH_SWF <- rep(NA, length(points))
points$classes_UKCEH_TCD10m <- rep(NA, length(points))
points$classes_UKCEH_TCD100m <- rep(NA, length(points))
points$classes_UKCEH_CORINE <- rep(NA, length(points))
points$classes_UKCEH_bluesky <- rep(NA, length(points))
points$classes_UKCEH_ODSE <- rep(NA, length(points))
points$classes_UKCEH_OSMLU <- NA
#deepforest
points$classes_UKCEH_DF25 <- NA
points$classes_UKCEH_DF50 <- NA
points$classes_UKCEH_DF1 <- NA
points$classes__UKCEH_DF3 <- NA
points$classes_UKCEH_DF5 <- NA
#fixing quickly
for(i in 1:length(points_all)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
               extent = ext(points_all[i,]))
  points_all$classes_UKCEH_TCD10m[i] <- mean(unlist(extract(TCD10m, area, ID = FALSE)))
  points_all$classes_UKCEH_TCD100m[i] <- mean(unlist(extract(TCD100m, area, ID = FALSE)))
}
for(i in 1:length(points_GB)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
               extent = ext(points_GB[i,]))
  points_GB$classes_UKCEH_TCD10m[i] <- mean(unlist(extract(TCD10m, area, ID = FALSE)))
  points_GB$classes_UKCEH_TCD100m[i] <- mean(unlist(extract(TCD100m, area, ID = FALSE)))
}
for(i in 1:length(points)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
               extent = ext(points[i,]))
  points$classes_UKCEH_TCD10m[i] <- mean(unlist(extract(TCD10m, area, ID = FALSE)))
  points$classes_UKCEH_TCD100m[i] <- mean(unlist(extract(TCD100m, area, ID = FALSE)))
}
#now I automate this for each point in silvopasture, for UKCEH map
for(i in 1:length(points)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
               extent = ext(points[i,]))
  points$classes_UKCEH[i] <- area$`_mode`
  points$classes_UKCEH_CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
  points$classes_UKCEH_ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
  points$classes_UKCEH_OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
  points$classes_UKCEH_TCD10m[i] <- mean(unlist(extract(TCD10m, area, ID = FALSE)))
  points$classes_UKCEH_TCD100m[i] <- mean(unlist(extract(TCD100m, area, ID = FALSE)))
  trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                extent = ext(area))
  trees_bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                        extent = ext(area))
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf2.gpkg",
                    extent = ext(area))
  trees_deepforest_25cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_25cm.gpkg",
                                extent = ext(area))
  trees_deepforest_50cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_50cm.gpkg",
                                extent = ext(area))
  trees_deepforest_1m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_1m.gpkg",
                              extent = ext(area))
  trees_deepforest_3m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_3m.gpkg",
                              extent = ext(area))
  trees_deepforest_5m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_5m.gpkg",
                              extent = ext(area))
  #getting the percentage treecover
  points$classes_UKCEH_TOW[i] <- (sum(expanse(intersect(trees, area)))/expanse(area))*100
  points$classes_UKCEH_bluesky[i] <- (sum(expanse(intersect(trees_bluesky, area)))/expanse(area))*100
  points$classes_UKCEH_SWF[i] <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #deepforest
  points$classes_UKCEH_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm, area)))/expanse(area))*100
  points$classes_UKCEH_DF50[i] <- (sum(expanse(intersect(trees_deepforest_50cm, area)))/expanse(area))*100
  points$classes_UKCEH_DF1[i] <- (sum(expanse(intersect(trees_deepforest_1m, area)))/expanse(area))*100
  points$classes__UKCEH_DF3[i] <- (sum(expanse(intersect(trees_deepforest_3m, area)))/expanse(area))*100
  points$classes_UKCEH_DF5[i] <- (sum(expanse(intersect(trees_deepforest_5m, area)))/expanse(area))*100
}


#CORINE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/CORINE_2018.tif")

#for OSM now
#landcovs
points$classes_OSM <- NA
points$classes_OSM_CORINE <- NA
points$classes_OSM_ODSE <- NA
points$classes_OSM_OSMLU <- NA
#trees
points$classes_OSM_TOW <- NA
points$classes_OSM_bluesky <- NA
points$classes_OSM_SWF <- NA
points$classes_OSM_TCD10m <- NA
points$classes_OSM_TCD100m <- NA
points$classes_OSM_deppforest <- NA
#deepforest
points$classes_OSM_DF25 <- NA
points$classes_OSM_DF50 <- NA
points$classes_OSM_DF1 <- NA
points$classes__OSM_DF3 <- NA
points$classes_OSM_DF5 <- NA
#now I automate this for each point in silvopasture, for UKCEH map
for(i in 1:length(points)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/OSMM_studysites/OSM_studysites.gpkg",
               extent = ext(points[i,]))
  if(length(area) != 0){
  points$classes_OSM[i] <- area$descriptiveterm
  points$classes_OSM_CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
  points$classes_OSM_ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
  points$classes_OSM_OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
  points$classes_OSM_TCD10m[i] <- mean(extract(TCD10m, area, ID = FALSE))
  points$classes_OSM_TCD100m[i] <- mean(extract(TCD100m, area, ID = FALSE))
  trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                extent = ext(area))
  trees_bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                        extent = ext(area))
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf2.gpkg",
                    extent = ext(area))

  #getting the percentage treecover
  points$classes_OSM_TOW[i] <- (sum(expanse(intersect(trees, area)))/expanse(area))*100
  points$classes_OSM_bluesky[i] <- (sum(expanse(intersect(trees_bluesky, area)))/expanse(area))*100
  points$classes_OSM_SWF[i] <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #deepforest
  points$classes_OSM_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm, area)))/expanse(area))*100
  points$classes_OSM_DF50[i] <- (sum(expanse(intersect(trees_deepforest_50cm, area)))/expanse(area))*100
  points$classes_OSM_DF1[i] <- (sum(expanse(intersect(trees_deepforest_1m, area)))/expanse(area))*100
  points$classes_OSM_DF3[i] <- (sum(expanse(intersect(trees_deepforest_3m, area)))/expanse(area))*100
  points$classes_OSM_DF5[i] <- (sum(expanse(intersect(trees_deepforest_5m, area)))/expanse(area))*100
}}
#saving this
writeVector(points, "study_sites/england_processed.gpkg",
            overwrite = TRUE)


length(which(is.na(points$classes_OSM))) #note 94 of these is not within a polygon
table(points$Type[which(is.na(points$classes_OSM))]) #note that this seems heavily in the WC control
table(points$Name[which(is.na(points$classes_OSM))]) #many in Shropshire and whitehall

table(points$classes_UKCEH[which(points$Type == "SP")]) #all imporved grassland
table(points$classes_UKCEH[which(points$Type == "SA")])#mostly improved grassland, some arable
table(points$classes_UKCEH[which(points$Type == "AC")])#mostly arable or imporved grassland, some rock
table(points$classes_UKCEH[which(points$Type == "WC")])#mostly woodland, some arable/grassland and some suburban



for(i in 1:length(points_all)) {
  if(points_all$Name[i] == "Loughgall") {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/BNG.gpkg",
                 extent = ext(points_all[i,]))
  } else {
    area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/lcm-2023-vec_6003876.gpkg",
                 extent = ext(points_all[i,]))
    
  }
  points_all$classes_UKCEH[i] <- area$`_mode`
  points_all$classes_UKCEH_CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
  points_all$classes_UKCEH_ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
  points_all$classes_UKCEH_OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
  points_all$classes_UKCEH_TCD10m[i] <- mean(extract(TCD10m, area, ID = FALSE))
  points_all$classes_UKCEH_TCD100m[i] <- mean(extract(TCD100m, area, ID = FALSE))
  trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                extent = ext(area))
  trees_bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                        extent = ext(area))
  trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf2.gpkg",
                    extent = ext(area))
  trees_deepforest_25cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_25cm.gpkg",
                                extent = ext(area))
  trees_deepforest_50cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_50cm.gpkg",
                                extent = ext(area))
  trees_deepforest_1m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_1m.gpkg",
                              extent = ext(area))
  trees_deepforest_3m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_3m.gpkg",
                              extent = ext(area))
  trees_deepforest_5m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_5m.gpkg",
                              extent = ext(area))
  #getting the percentage treecover
  points_all$classes_UKCEH_TOW[i] <- (sum(expanse(intersect(trees, area)))/expanse(area))*100
  points_all$classes_UKCEH_bluesky[i] <- (sum(expanse(intersect(trees_bluesky, area)))/expanse(area))*100
  points_all$classes_UKCEH_SWF[i] <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
  #deepforest
  points_all$classes_UKCEH_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm, area)))/expanse(area))*100
  points_all$classes_UKCEH_DF50[i] <- (sum(expanse(intersect(trees_deepforest_50cm, area)))/expanse(area))*100
  points_all$classes_UKCEH_DF1[i] <- (sum(expanse(intersect(trees_deepforest_1m, area)))/expanse(area))*100
  points_all$classes__UKCEH_DF3[i] <- (sum(expanse(intersect(trees_deepforest_3m, area)))/expanse(area))*100
  points_all$classes_UKCEH_DF5[i] <- (sum(expanse(intersect(trees_deepforest_5m, area)))/expanse(area))*100
  
}

writeVector(points_all, "study_sites/pall_processed.gpkg",
            overwrite = TRUE)

#for OSM
#now I automate this for each point in silvopasture, for UKCEH map
for(i in 1:length(points_GB)) {
  area <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/OSMM_studysites/OSM_studysites_GB.gpkg",
               extent = ext(points_GB[i,]))
  if(length(area) != 0){
    points_GB$classes_OSM[i] <- area$descriptiveterm
    points_GB$classes_OSM_CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
    points_GB$classes_OSM_ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
    points_GB$classes_OSM_OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
    points_GB$classes_OSM_TCD10m[i] <- mean(extract(TCD10m, area, ID = FALSE))
    points_GB$classes_OSM_TCD100m[i] <- mean(extract(TCD100m, area, ID = FALSE))
    trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                  extent = ext(area))
    trees_bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                          extent = ext(area))
    trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf2.gpkg",
                      extent = ext(area))
    trees_deepforest_25cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_25cm.gpkg",
                                  extent = ext(area))
    trees_deepforest_50cm <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_50cm.gpkg",
                                  extent = ext(area))
    trees_deepforest_1m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_1m.gpkg",
                                extent = ext(area))
    trees_deepforest_3m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_3m.gpkg",
                                extent = ext(area))
    trees_deepforest_5m <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/trees/trees_5m.gpkg",
                                extent = ext(area))
    #getting the percentage treecover
    points_GB$classes_OSM_TOW[i] <- (sum(expanse(intersect(trees, area)))/expanse(area))*100
    points_GB$classes_OSM_bluesky[i] <- (sum(expanse(intersect(trees_bluesky, area)))/expanse(area))*100
    points_GB$classes_OSM_SWF[i] <- (sum(expanse(intersect(trees_SWF, area)))/expanse(area))*100
    #deepforest
    points_GB$classes_OSM_DF25[i] <- (sum(expanse(intersect(trees_deepforest_25cm, area)))/expanse(area))*100
    points_GB$classes_OSM_DF50[i] <- (sum(expanse(intersect(trees_deepforest_50cm, area)))/expanse(area))*100
    points_GB$classes_OSM_DF1[i] <- (sum(expanse(intersect(trees_deepforest_1m, area)))/expanse(area))*100
    points_GB$classes__OSM_DF3[i] <- (sum(expanse(intersect(trees_deepforest_3m, area)))/expanse(area))*100
    points_GB$classes_OSM_DF5[i] <- (sum(expanse(intersect(trees_deepforest_5m, area)))/expanse(area))*100
    
  }}

writeVector(points_GB, "study_sites/GB_processed.gpkg",
            overwrite = TRUE)

#plotting these out
library(ggplot2)
library(ggpubr)
library(shadowtext)


UKCEH_labs <- c("Woodland", "Rock", "Woodland", "Suburban", 
                "Arable", "Pasture", "Grassland")
UKCEH_cols <- c("#0C6805", "#D3D0FD", "#828282", "#642722",
                "#e6e64d", "#09FA0B")
CORINE_cols <- c("#ffe6ff", "#642722", "#e6e64d", "#0C6805",
                 "#09FA0B", "#6ea3d5", "#80f2e6", "black")
#
#classing
points_all$UKCEH <- factor(points_all$classes_UKCEH, levels = c(1, 12, 2, 21, 3, 4, 5),
                       labels = UKCEH_labs)
points_GB$classes_OSM[which(points_GB$classes_OSM == "")] <- NA
points_GB$OSM2 <- factor(points_GB$classes_OSM, labels = c("Agriculture", "Trees", "Trees",
                                             "Trees", "Trees","Grassland", "Multi Surface", 
                                             "Trees", "Trees", "Trees", "Trees","Trees",  "Orchard",
                                             "Grassland", "Grassland", "Trees", "Trees"))
points_all$UKCEH_CORINE <- factor(points_all$classes_UKCEH_CORINE, labels = c("Sports", "Arable",
                                                              "Pasture", "Trees",
                                                              "Trees", "Trees", "Grassland",
                                                              "Water", "Moores", "Mines"))
points_GB$OSM_CORINE <- factor(points_GB$classes_OSM_CORINE, levels = c(11, 12, 18, 23, 24, 25, 26, 27, 41, 7),
                            labels = c("Sports", "Arable",
                                                              "Pasture", "Trees",
                                                              "Trees", "Trees", "Grassland",
                                                              "Water", "Moores", "Mines"))
points_all$OSMLU <- factor(points_all$classes_UKCEH_OSMLU, levels = c(11, 13,14, 21, 22, 23, 31, 32, 41),
                           labels = c("Urban", "Mines", "Artificial", "Arable", "Orchard", 
                                      "Pasture", "Trees", "Shrub", "Wetlands"))
points_GB$OSMLU <- factor(points_GB$classes_OSM_OSMLU, levels = c(11, 13,14, 21, 22, 23, 31, 32, 41),
                           labels = c("Urban", "Mines", "Artificial", "Arable", "Orchard", 
                                      "Pasture", "Trees", "Shrub", "Wetlands"))
OSMLU_cols <- c("white", "black", "grey", "#642722", "#0C8000", 
                "#e6e64d", "#0C6805", "#09FA0B", "#6ea3d5")
OSM_cols <- c("#642722", "#0C6805", "#09FA0B", "#060600", "#0C8000")
points_all$UKCEH_ODSE <- factor(points_all$classes_UKCEH_ODSE, levels = c(15, 16, 17,18, 19, 21, 8, 5, 9),
                            labels = c("Pasture", "Trees", "Trees", "Grassland",
                                      "Moores", "Trees", "Urban", "Mines", "Arable"))
points_GB$OSM_ODSE <- factor(points_GB$classes_OSM_ODSE, levels = c(15, 16, 17, 18, 19, 21, 8,5, 9),
                          labels = c("Pasture", "Trees", "Trees", "Grassland",
                                     "Moores", "Trees", "Urban", "Mines", "Arable"))
ODSE_cols <- c("#e6e64d", "#0C6805", "#09FA0B", "#6ea3d5", "white", "black", "#642722")
OSMLU_cols

points_all_data <- as.data.frame(points_all)
points_GB_data <- as.data.frame(points_GB)
A.1 <- ggplot(data = points_all_data, aes(fill = UKCEH, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = UKCEH_cols) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UKCEH")
A.2 <- ggplot(data = points_all_data, aes(fill = UKCEH_CORINE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = CORINE_cols) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UKCEH CORINE")
A.3 <- ggplot(data = points_all_data, aes(fill = UKCEH_ODSE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = ODSE_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UKCEH ODSE")
A.4 <- ggplot(data = points_GB_data, aes(fill = OSM2, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = OSM_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSMM")
A.5 <- ggplot(data = points_GB_data, aes(fill = OSM_CORINE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = CORINE_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSMM CORINE")
A.6 <- ggplot(data = points_GB_data, aes(fill = OSM_ODSE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "UKCEH", values = ODSE_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSMM ODSE")
A.7 <- ggplot(data = points_GB_data, aes(fill = OSMLU, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "OSMLU", values = OSMLU_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSMM OSMLU")
B.8 <- ggplot(data = points_all_data, aes(fill = OSMLU, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "OSMLU", values = OSMLU_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UKCEH OSMLU")

ggarrange(A.1, A.4, A.2, A.5, A.3, A.6, A.8, A.7, ncol =2, nrow = 4,
          labels = c("A1", "B1", "A2", "B2", "A3", "B3"))

ggsave(filename = "figures/landovers_full.jpeg", units = "mm", width =210, height = 260)


#for IGARSS conference
B.1 <- ggplot(data = points_all_data, aes(fill = UKCEH, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3.5) +
  scale_fill_manual(name = "UKCEH", values = UKCEH_cols) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 18),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UKCEH")
B.2 <- ggplot(data = points_all_data, aes(fill = UKCEH_CORINE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3.5) +
  scale_fill_manual(name = "UKCEH", values = CORINE_cols) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 18),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("CORINE")
B.3 <- ggplot(data = points_all_data, aes(fill = UKCEH_ODSE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3.5) +
  scale_fill_manual(name = "UKCEH", values = ODSE_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 18),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("ODSE")
B.8 <- ggplot(data = points_all_data, aes(fill = OSMLU, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3.5) +
  scale_fill_manual(name = "OSMLU", values = OSMLU_cols, drop = FALSE) +
  theme(legend.position = "top",
        axis.text.y=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 18),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSMLU")
ggarrange(B.1, B.2, B.3, B.8, ncol = 2, nrow =2, labels = c("A", "B", "C", "D"))
#csving
ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/conferences/igarss/landcovers_test.png", 
       units = "mm", width =280, height = 280, dpi = 900)


#okay onto treecover, assumed the ODSE+UKCEH combination
points_data <- as.data.frame(points)
#now doing the treecovers. There should only be 2?
points$Planted <- as.numeric(points$Planted)
ag <- which(points$classes_UKCEH_ODSE == "9")
pa <- which(points$classes_UKCEH_ODSE == "15")
B.1 <- ggplot(data = points_data[ag,], aes(x = Type, fill = Type, y = classes_UKCEH_TOW)) +
  geom_violin()
B.2 <- ggplot(data = points_data[pa,], aes(x = Type, fill = Type, y = classes_UKCEH_TOW)) +
  geom_violin()
#okay maybe a 1-5% might be best, let's try 1% first (again just for igarss rn)

#for all (UKCEH, ODSE, OSMLU, CORINE, and trees: SWF, TCD10, TCD100, DF(x5))
points_all_results <- data.frame(Landcov = rep(NA, 8*4), Treecov=NA, SA =NA, SA_mat= NA, SP = NA, 
                                 SP_mat = NA,
                                 AC = NA, WC =NA)
points_all_results$Landcov <- c(rep("UKCEH", 8), rep("CORINE", 8),
                                rep("ODSE", 8), rep("OSMLU", 8))
points_all_results$Treecov <- rep(c("TCD10m", "TCD100m", "SWF", "DF25",
                                  "DF50", "DF1", "DF3", "DF5"), 4)
lands <- c(23, 24, 25, 26) #col numbers for landcovers, in oder
pall_dat <- as.data.frame(points_all)
trees <- c(13, 14, 17, 18, 19, 20, 21, 22)

for(n in 1:4){ #landcovers
    
 for(j in 1:8) {#treecovs
    #SA
    points_all_results[(j + 8*(n-1)),3] <- length(which((pall_dat[which(points_all$Type == "SA"),lands[n]] ==
                                                    "Pasture" | pall_dat[which(points_all$Type == "SA"),lands[n]] ==
                                                     "Arable") & (pall_dat[which(points_all$Type == "SA"),trees[j]] >5 &
                                                                   pall_dat[which(points_all$Type == "SA"),trees[j]] <25)))/length(which(points_all$Type == "SA"))*100
    #SAmat
    points_all_results[j + 8*(n-1),4] <- length(which((pall_dat[which(points_all$Type == "SA" & points_all$Planted < 2015),lands[n]] ==
                                                     "Pasture" | pall_dat[which(points_all$Type == "SA"
                                                                                & points_all$Planted < 2015),lands[n]] ==
                                                     "Arable") & (pall_dat[which(points_all$Type == "SA"
                                                                                 & points_all$Planted < 2015),trees[j]] >5 &
                                                                    pall_dat[which(points_all$Type == "SA"
                                                                                   & points_all$Planted < 2015),trees[j]] <25)))/length(which(points_all$Type == "SA"
                                                                                                                                              & points_all$Planted < 2015))*100
    #SP
    points_all_results[j + 8*(n-1),5] <- length(which((pall_dat[which(points_all$Type == "SP"),lands[n]] ==
                                                     "Pasture" | pall_dat[which(points_all$Type == "SP"),lands[n]] ==
                                                     "Arable") & (pall_dat[which(points_all$Type == "SP"),trees[j]] >5 &
                                                                    pall_dat[which(points_all$Type == "SP"),trees[j]] <25)))/length(which(points_all$Type == "SP"))*100
    #spmat
    points_all_results[j + 8*(n-1),6] <- length(which((pall_dat[which(points_all$Type == "SP" & points_all$Planted < 2015),lands[n]] ==
                                                     "Pasture" | pall_dat[which(points_all$Type == "SP"
                                                                                & points_all$Planted < 2015),lands[n]] ==
                                                     "Arable") & (pall_dat[which(points_all$Type == "SP"
                                                                                 & points_all$Planted < 2015),trees[j]] >5 &
                                                                    pall_dat[which(points_all$Type == "SP"
                                                                                   & points_all$Planted < 2015),trees[j]] <25)))/length(which(points_all$Type == "SP"
                                                                                                                                              & points_all$Planted < 2015))*100
    #AC
    points_all_results[j + 8*(n-1),7] <- length(which((pall_dat[which(points_all$Type == "AC"),lands[n]] !=
                                                     "Pasture" & pall_dat[which(points_all$Type == "AC"),lands[n]] !=
                                                     "Arable") |
                                                    pall_dat[which(points_all$Type == "AC"),trees[j]] <5 |
                                              pall_dat[which(points_all$Type == "AC"),trees[j]] >25))/length(which(points_all$Type == "AC"))*100
    #WC
    points_all_results[j + 8*(n-1),8] <- length(which((pall_dat[which(points_all$Type == "WC"),lands[n]] !=
                                                     "Pasture" & pall_dat[which(points_all$Type == "WC"),lands[n]] !=
                                                     "Arable") |
                                                    pall_dat[which(points_all$Type == "WC"),trees[j]] <5 |
                                              pall_dat[which(points_all$Type == "WC"),trees[j]] >25))/length(which(points_all$Type == "WC"))*100
  }}
points_all_results$all <- points_all_results$SA*0.1162791 + points_all_results$SP*0.232558 +
  points_all_results$AC*0.3255814 + points_all_results$WC*0.3255814
