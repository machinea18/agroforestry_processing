library(terra)
studysites <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/studysites_extra.gpkg")

#landcover maps
studysites$Type[which(studysites$Type == "Woodpasture")] <- "SP"
#removing anything too young (2020)
studysites <- studysites[-which(studysites$Name %in% 
                                  studysites$Name[which(studysites$Planted>2020)]),]
studysites <- makeValid(studysites)

#rasters
CORINE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/CORINE_2018.tif")
ODSE <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/landcovs/ODSE_2020.tif")
OSMLU <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc//OSMLU/OSMLU.tif")
UKCEH <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/UKCEH.tif")
#trees
TCD10m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree10m2.tif")
TCD100m <- rast("C:/Users/am1355/OneDrive - University of Leicester/euraf stuff/cropped/tree100m2.tif")
ETH <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/ETH_CHM_2020.tif")
PS <- rast("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/PS_trees")
#PS <- project(PS, crs(CORINE)) #reprojecting, started at 10:57
#note the rdata has been saved so that this reprojection does not need to be done again

#pixel based results
pixel_results <- data.frame(Type = NA, Planted = NA, 
                            Name = NA, CORINE = NA, ODSE = NA,
                            OSMLU = NA, UKCEH = NA, TCD10m = 1, TCD100m = 1,
                            ETH = 1, CHM = 1)

study_names <- unique(studysites$Name)
meta_names <- c("Henfaes.tif", "Parkhill.tif", 
                "Whitehall.tif", "Dartington.tif", "Tolhurst.tif", 
                "wakelyns.tif", "Shropshire.tif", "FarmED.tif",
                "ThreeHagges.tif", "LittleHidden.tif", 
                "Henbant.tif", "Pembrokeshire.tif",
                "Loddington.tif", "Elm.tif", "Eastbrook.tif",
                "Farrochil.tif", "Gibside.tif", "TempleFields.tif",
                "Allerton.tif", "Maple.tif", "TRTame.tif",
                "WoodValley.tif", "Loughgall.tif", "Kilowna.tif", 
                "Gibbson.tif")

#extract(CORINE, studysites[,1], ID = FALSE)

for(i in 1:length(studysites)){
#rewriting all of the rasters
  CHM <- rast(paste("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/meta_CHM/",
                    meta_names[which(study_names == studysites$Name[i])], 
                    sep = ""))
  CORINE_tmp <- resample(CORINE, CHM,
                         method = "near")
  ODSE_tmp <- resample(ODSE, CHM,
                       method = "near")
  OSMLU_tmp <- resample(OSMLU, CHM,
                        method = "near")
  UKCEH_tmp <- resample(UKCEH, CHM,
                        method = "near")
  TCD10m_tmp <- resample(TCD10m, CHM,
                         method = "near")
  TCD100m_tmp <- resample(TCD100m, CHM,
                          method = "near")
  ETH_tmp <- resample(ETH, CHM,
                      method = "near")
  #getting these together
  tmp <- data.frame(Type = studysites$Type[i], Planted = studysites$Planted[i], 
                    Name = studysites$Name[i], 
                    CORINE = extract(CORINE_tmp, studysites[i,],
                                                                ID = FALSE),
                    ODSE = extract(ODSE_tmp, studysites[i,],
                                   ID = FALSE),
                    OSMLU = extract(OSMLU_tmp, studysites[i,],
                                    ID = FALSE), 
                    UKCEH = extract(UKCEH_tmp, studysites[i,],
                                    ID = FALSE), 
                    TCD10m = extract(TCD10m_tmp, studysites[i,],
                                     ID = FALSE), 
                    TCD100m = extract(TCD100m_tmp, studysites[i,],
                                      ID = FALSE),
                    ETH = extract(ETH_tmp, studysites[i,],
                                  ID = FALSE), 
                    CHM = extract(CHM, studysites[i,],
                                  ID = FALSE))
  #remaming
  names(tmp) <- names(pixel_results)
  #binding
  pixel_results <- rbind(pixel_results, tmp)
}

#removing first line
pixel_results2 <- pixel_results[-1,]

#setting factors 
pixel_results2$CORINE <- factor(pixel_results2$CORINE,
       levels = c(2,7,11,12,18,20,21,23,
                  24,25,26,41),
       labels = c("Other", "Other", "Artificial Vegetation", "Arable",
                  "Pasture", "Arable",
                  "Arable", "Forest", "Forest", 
                  "Forest", "Grass", "Other"))
pixel_results2$ODSE <- factor(pixel_results2$ODSE,
                              levels = c(1,5, 8,9, 13, 15,16,17,
                                         18,19,21,27,28),
                              labels = c("Other", "Other", "Artificial Vegetation", "Arable","Orchard", 
                                         "Pasture", "Forest", "Forest", 
                                         "Grass", "Moore", "Forest", "Wetland",
                                         "Wetland"))
pixel_results2$OSMLU <- factor(pixel_results2$OSMLU,
                               levels = c(5,11, 12, 13, 14, 21,
                                          22, 23, 31, 32, 33, 41, 50,51),
                               labels = c("Other", "Other",
                                          "Other", "Other",
                                          "Artificial Vegetation",
                                          "Arable", "Orchard", "Pasture",
                                          "Forest", "Shrub", "Other", "Wetland",
                                          "Grass", "Moore"))
pixel_results2$UKCEH <- factor(pixel_results2$UKCEH, 
                               levels = c(1:11, 20, 21),
                               labels = c("Forest", "Forest", "Arable",
                                          "Pasture", "Grass", "Grass", "Grass",
                                          "Wetland", "Shrub", "Grass", "Wetland",
                                          "Other", "Other"))

#defining colour palettes
CORINE_cols <- c("#828282", "black", "#732600", "#998100", "#006600",
                 "#00ff00")
ODSE_cols <- c("#828282", "black", "#732600", "#58d5b6", "#998100", "#006600",
               "#00ff00", "#cb2ee3", "#141fe9")
UKCEH_cols <- c("#006600","#732600",  "#998100", "#00ff00", "#4d95ee",
                "#141fe9", "#828282")
OSMLU_cols <- c("#828282", "black", "#732600", "#58d5b6",  "#998100", "#006600",
                "#141fe9", "#4d95ee", "#00ff00", "#cb2ee3")

#putting these together:
library(ggplot2)
library(ggpubr)
library(shadowtext)

plot_A <- ggplot(data = pixel_results2, aes(fill = UKCEH, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', 
                  aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..],
                                              accuracy = 0.1)), 
                  position = position_fill(vjust = .5), size = 3) +
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

plot_B <- ggplot(data = pixel_results2, aes(fill = CORINE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', 
                  aes(label =scales::percent(..count../tapply(..count.., ..x.., sum)[..x..],
                                             accuracy = 0.1)), 
                  position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "CORINE", values = CORINE_cols) +
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
  ggtitle("CORINE")

plot_C <- ggplot(data = pixel_results2, aes(fill = ODSE, x = Type)) +
  geom_bar(position = "fill") +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', 
                  aes(label =scales::percent(..count../tapply(..count.., ..x.., sum)[..x..],
                                             accuracy = 0.1)), 
                  position = position_fill(vjust = .5), size = 3) +
  scale_fill_manual(name = "ODSE", values = ODSE_cols) +
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
  ggtitle("ODSE-LU")
plot_D <- ggplot(data = pixel_results2, aes(fill = OSMLU, x = Type)) +
  geom_bar(position = "fill",
           show.legend = TRUE) +
  theme_minimal()+
  guides(fill=guide_legend(nrow=2)) +
  geom_shadowtext(stat='count', 
                  aes(label =scales::percent(..count../tapply(..count.., ..x.., sum)[..x..],
                                             accuracy = 0.1)), 
                  position = position_fill(vjust = .5), size = 3) +
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
  ggtitle("OSMLU")

#adding together
ggarrange(plot_A, plot_B, plot_C, plot_D,
          ncol =2, nrow =2, common.legend = TRUE, labels = c("A", "B",
                                            "C", "D"),
          legend.grob = get_legend(plot_D),
          legend = "bottom")

ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/figures/landcovs.pdf",
       width = 15, height = 15, units = "cm", dpi = 600)
#need pdf so i can alter it in adobe later

#now doing the treecover stuff over time



#parcel based
   parcel_results <- data.frame(CORINE = rep(NA, length(studysites)),
                               ODSE = NA,
                               OSMLU = NA,
                               UKCEH = NA,
                               TCD10m = NA,
                               TCD100m = NA,
                               ETH = 1,
                               PS = 1,
                               TOW = 1,
                               SWF = 1,
                               CHM = 1,
                               bluesky = 1,
                               DF = 1,
                               DF_trained = 1,
                               Type = NA,
                               Name = NA,
                               Country= NA,
                               Planted = 1)
  for(i in 1:length(studysites)) {
    parcel_results$Type[i] <- studysites$Type[i]
    parcel_results$Name[i] <- studysites$Name[i]
    parcel_results$Country[i] <- studysites$Country[i]
    parcel_results$Planted[i] <- studysites$Planted[i]
    
    #using a negative buffer
    area <- buffer(studysites[i,], width = -5)
    parcel_results$CORINE[i] <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
    parcel_results$ODSE[i] <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
    parcel_results$OSMLU[i] <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
    parcel_results$UKCEH[i] <- names(sort(-table(extract(UKCEH, area, ID = FALSE))))[1]
    #trees
    parcel_results$TCD10m[i] <- zonal(TCD10m, area, weights = TRUE)
    parcel_results$TCD100m[i] <- zonal(TCD100m, area, weights = TRUE)
    parcel_results$ETH[i] <- (sum(as.numeric(extract(ETH, area, ID = FALSE)>10))/length(unlist(
      extract(ETH, area, ID = FALSE))))*100
    parcel_results$PS[i] <- (sum(as.numeric(extract(PS, area, ID = FALSE)>=10))/length(unlist(
      extract(PS, area, ID = FALSE))))*100
    #for CHM
    CHM <- rast(paste("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/meta_CHM/",
                      meta_names[which(study_names == studysites$Name[i])], 
                      sep = ""))
    
    #parcel_results$CHM[i] <- zonal(CHM, area, weights = TRUE)
    parcel_results$CHM[i] <- (sum(as.numeric(extract(CHM, area, ID = FALSE)>=10))/length(unlist(
      extract(CHM, area, ID = FALSE))))*100
    #for vectors
    #TOW
    TOW <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
         extent = ext(area))
    parcel_results$TOW[i] <- (sum(expanse(terra::intersect(TOW, area)))/expanse(area))*100
    #SWF
    SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                extent = ext(area))
    parcel_results$SWF[i] <- (sum(expanse(terra::intersect(SWF, area)))/expanse(area))*100
    #DF
    DF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/trees_extra.gpkg",
              extent = ext(area))
    DF <- erase(DF) #to remove overlapping parts
    parcel_results$DF[i] <- (sum(expanse(terra::intersect(DF, area)))/expanse(area))*100
    DF2 <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/DF_trees_trained2.gpkg",
                extent = ext(area))
    DF2 <- erase(DF2)
    parcel_results$DF_trained[i] <- (sum(expanse(terra::intersect(DF2, area)))/expanse(area))*100
    #bluesky
    bluesky <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/bluesky_NTM_full.gpkg",
                    extent = ext(area))
    parcel_results$bluesky[i] <- (sum(expanse(terra::intersect(bluesky, area)))/expanse(area))*100
  }
  #setting numeric
   parcel_results$Planted <- as.numeric(parcel_results$Planted)
   parcel_results$TCD10m <- as.numeric(parcel_results$TCD10m)
   parcel_results$TCD100m <- as.numeric(parcel_results$TCD100m)
   parcel_results$ETH <- as.numeric(parcel_results$ETH)
   parcel_results$CHM <- as.numeric(parcel_results$CHM)
   parcel_results$PS <- as.numeric(parcel_results$PS)
   parcel_results$TOW <- as.numeric(parcel_results$TOW)
   parcel_results$SWF <- as.numeric(parcel_results$SWF)
   parcel_results$DF <- as.numeric(parcel_results$DF)
   parcel_results$DF_trained <- as.numeric(parcel_results$DF_trained)

#getting the plots of treecover over time in SA and SP sites
   #TCD10m
TCD10m_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
       aes(x = Planted, y = TCD10m)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("TCD\n10m (%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#TD100m
TCD100m_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
       aes(x = Planted, y = TCD100m)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("TCD\n100m (%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#ETH
ETH_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                       aes(x = Planted, y = ETH)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  #ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("ETH - CHM\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#Meta-CHM
Meta_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                       aes(x = Planted, y = CHM)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("Meta - CHM\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#Planetscope
PS_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                  aes(x = Planted, y = PS)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("PS-CHM\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#TOW
TOW_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP") &
                                                 parcel_results$Country == "England"),],
                    aes(x = Planted, y = TOW)) + #note needs to only include points in england due to coverage
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("TOW\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#SWF
SWF_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                   aes(x = Planted, y = SWF)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("SWF\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#NTM
NTM_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP") &
                                                 parcel_results$Country != "Northern Ireland"),],
                  aes(x = Planted, y = bluesky)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("NTM\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#DF
DF_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                   aes(x = Planted, y = DF)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("DeepForest\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
#DF-trained
DF_trained_plot <- ggplot(data = parcel_results[which(parcel_results$Type %in% c("SA", "SP")),],
                  aes(x = Planted, y = DF_trained)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, col = "red") +
  ylim(0,100) +
  geom_point() +
  theme_bw()+
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  ylab("DF-trained\n(%)") +
  facet_wrap(~Type) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha = 0.5)
   
#arranging these together
ggarrange(TCD10m_plot, TCD100m_plot, ETH_plot, Meta_plot, PS_plot,
          SWF_plot, TOW_plot, NTM_plot,
          DF_plot, DF_trained_plot,
          ncol = 2, nrow = 5, labels = c("A", "B", "C", "D", "E", "F", "G",
                                         "H", "I", "J"))

#note currently spot empty for a bluesky map. Awaiting data
ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/figures/treecovs_age.png",
       width = 21, height = 32/2, units = "cm", dpi = 600)




###FINAL OMISSION AND COMISSION CODE
#new code for OSM/UKCEH parcels by area

parcel_results <- data.frame(CORINE = NA,
                             ODSE = NA,
                             OSMLU = NA,
                             UKCEH = NA,
                             TCD10m = NA,
                             TCD100m = NA,
                             ETH = 1,
                             TOW = 1,
                             SWF = 1,
                             CHM = 1,
                             DF = 1,
                             Type = NA,
                             Name = NA,
                             Country= NA,
                             Planted = 1,
                             overlap_a = 1)
#Meta-CHM

for(i in 1:length(studysites)) {
  if(studysites$Country[i] == "Northern Ireland") {
    parcels <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/UKCEH_2023/lcm-2023-vec_6003876/BNG.gpkg",
                    extent = ext(studysites[i,]))
    #getting only intersecting ones
    parcels <- parcels[!relate(parcels, studysites[i,], "disjoint"),]
  } else {
    parcels <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/OSMM_studysites/OSM_studysites_extra.gpkg",
                    extent = ext(studysites[i,]))
    #intersections
    parcels <- parcels[!relate(parcels, studysites[i,], "disjoint"),]
  }
  for(j in 1:length(parcels)) {
    temp <- parcel_results[1,]
    temp$overlap_a <- expanse(terra::intersect(parcels[j,], studysites[i,]))
    area <- buffer(parcels[j,], width = 0)
    area2 <- buffer(parcels[j,], width = 5) # for the treecover analsis
    #other stuff
    temp$Name <- studysites$Name[i]
    temp$Country <- studysites$Country[i]
    temp$Type <- studysites$Type[i]
    temp$Planted <- studysites$Planted[i]
    if(length(expanse(area)) != 0) {
    #getting the majority of the landcovs
    temp$CORINE <- names(sort(-table(extract(CORINE, area, ID = FALSE))))[1]
    temp$ODSE <- names(sort(-table(extract(ODSE, area, ID = FALSE))))[1]
    temp$OSMLU <- names(sort(-table(extract(OSMLU, area, ID = FALSE))))[1]
    temp$UKCEH <- names(sort(-table(extract(UKCEH, area, ID = FALSE))))[1]
    
    #getting the treecovs
    #the vectors
    trees <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/TOW/TOW_all.gpkg",
                  extent = ext(area2))
    trees_SWF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/swf.gpkg",
                      extent = ext(area2))
    DF <- vect("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/trees_extra.gpkg",
               extent = ext(area2))
     #meta
    meta_trees <- rast(paste("C:/Users/am1355/OneDrive - University of Leicester/misc/trees/meta_CHM/",
                      meta_names[which(study_names == 
                                          studysites$Name[i])], 
                      sep = ""))
    #finally the treecovs
    temp$TCD10m <- unlist(zonal(TCD10m, area2, weights = TRUE))
    temp$TCD100m <- unlist(zonal(TCD100m, area2, weights = TRUE))
    temp$ETH <- (sum(as.numeric(extract(ETH, area2, ID = FALSE)>=3))/length(unlist(
      extract(ETH, area, ID = FALSE))))*100
    #temp$ETH <- unlist(zonal(ETH, area2, weights = TRUE))
    temp$CHM <- (sum(as.numeric(extract(meta_trees, area2, ID = FALSE)>=3))/length(unlist(
      extract(meta_trees, area2, ID = FALSE))))*100
    #temp$CHM <- unlist(zonal(meta_trees, area2, weights = TRUE))
    temp$TOW <- (sum(expanse(terra::intersect(trees, area2)))/expanse(area2))*100
    temp$SWF <- (sum(expanse(terra::intersect(trees_SWF, area2)))/expanse(area2))*100
    temp$DF <- (sum(expanse(terra::intersect(DF, area2)))/expanse(area2))*100

    
    #parcel_results <- rbind(parcel_results, temp)
    }
    parcel_results <- rbind(parcel_results, temp)
    }
}
parcel_results2 <- parcel_results[-1,]
#changing names
parcel_results2$CORINE <- factor(parcel_results2$CORINE,
                                levels = c(2,7,11,12,18,20,21,23,
                                           24,25,26,41),
                                labels = c("Other", "Other", "Artificial Vegetation", "Arable",
                                           "Pasture", "Arable",
                                           "Arable", "Forest", "Forest", 
                                           "Forest", "Grass", "Other"))
parcel_results2$ODSE <- factor(parcel_results2$ODSE,
                              levels = c(1,5, 8,9, 13, 15,16,17,
                                         18,19,21,27,28),
                              labels = c("Other", "Other", "Artificial Vegetation", "Arable","Orchard", 
                                         "Pasture", "Forest", "Forest", 
                                         "Grass", "Moore", "Forest", "Wetland",
                                         "Wetland"))
parcel_results2$OSMLU <- factor(parcel_results2$OSMLU,
                               levels = c(5,11, 12, 13, 14, 21,
                                          22, 23, 31, 32, 33, 41, 50,51),
                               labels = c("Other", "Other",
                                          "Other", "Other",
                                          "Artificial Vegetation",
                                          "Arable", "Orchard", "Pasture",
                                          "Forest", "Shrub", "Other", "Wetland",
                                          "Grass", "Moore"))
parcel_results2$UKCEH <- factor(parcel_results2$UKCEH, 
                               levels = c(1:11, 20, 21),
                               labels = c("Forest", "Forest", "Arable",
                                          "Pasture", "Grass", "Grass", "Grass",
                                          "Wetland", "Shrub", "Grass", "Wetland",
                                          "Other", "Other"))







#getting commission and omissions errors

#pixel based (only including TCD, CHM, and ETH here)

pixel_errors <- data.frame(Landcov = c(rep("UKCEH", 4),
                                       rep("CORINE", 4),
                                       rep("ODSE-LU",4),
                                       rep("OSMLU", 4)),
                           Treecov = rep(c("TCD10m", "TCD100m",
                                           "Meta-CHM", "ETH-CHM"), 4),
                           Omission = NA, Comission = NA,
                           mat_omission = NA)


treecov_nums <- rep(c(8,9,11,10),4)
landcov_nums <- c(rep(7,4), rep(4,4),
                  rep(5,4), rep(6,4))

pixel_ag <- pixel_results2[pixel_results2$Type == "SA" |
                             pixel_results2$Type == "SP",]
pixel_oth <- pixel_results2[!(pixel_results2$Type %in% c("SA", "SP")),]

for(i in 1:nrow(pixel_errors)){

  #mature comission error only including plantings prior to 2000
  if(i %in% c(3,4,7,8, 11,12,15,16)) {
    #note this is the CHM, so the threshold is 1m rather than 5%
    #omission (1m threshold)
    pixel_errors[i,3] <- length(which(!(pixel_ag[,landcov_nums[i]] %in% c("Arable", "Pasture")) |
                                        (pixel_ag[,landcov_nums[i]] %in% c("Arable",
                                                                           "Pasture") &
                                           pixel_ag[,treecov_nums[i]] < 5
                                        ))) / nrow(pixel_ag)
    #omission mature
    pixel_errors[i,5] <- length(which(!(pixel_ag[pixel_ag$Planted < 2000,landcov_nums[i]] %in% c("Arable", "Pasture")) |
                                        (pixel_ag[pixel_ag$Planted < 2000,landcov_nums[i]] %in% c("Arable",
                                                                           "Pasture") &
                                           pixel_ag[pixel_ag$Planted < 2000,treecov_nums[i]] < 5
                                        )))/nrow(pixel_ag[which(pixel_ag$Planted < 2000),])
    #comission errors
    pixel_errors[i,4] <- length(which(pixel_oth[, landcov_nums[i]] %in% c("Arable", "Pasture") &
                                        pixel_oth[treecov_nums[i]] >= 1))/nrow(pixel_oth)

  } else {
    #treecover densitys (5% threshold)
    
    pixel_errors[i,3] <- length(which(!(pixel_ag[,landcov_nums[i]] %in% c("Arable", "Pasture")) |
                                        (pixel_ag[,landcov_nums[i]] %in% c("Arable",
                                                                           "Pasture") &
                                           pixel_ag[,treecov_nums[i]] < 5
                                        ))) / nrow(pixel_ag)
    #omission mature
    pixel_errors[i,5] <- length(which(!(pixel_ag[pixel_ag$Planted < 2000,landcov_nums[i]] %in% c("Arable", "Pasture")) |
                                        (pixel_ag[pixel_ag$Planted < 2000,landcov_nums[i]] %in% c("Arable",
                                                                                                  "Pasture") &
                                           pixel_ag[pixel_ag$Planted < 2000,treecov_nums[i]] < 5
                                        )))/nrow(pixel_ag[which(pixel_ag$Planted < 2000),])
    #comission errors
    pixel_errors[i,4] <- length(which(pixel_oth[, landcov_nums[i]] %in% c("Arable", "Pasture") &
                                        pixel_oth[treecov_nums[i]] >= 5))/nrow(pixel_oth)

  }
}
#getting a very nice and clean table
pixel_error_publish <- data.frame(pixel_errors$Landcov, pixel_errors$Treecov,
                                  Omission = round(pixel_errors$Omission, 3)*100,
                                  mat_omission = round(pixel_errors$mat_omission, 3)*100,
                                  Comission = round(pixel_errors$Comission, 3)*100)


#parcel based (this is much harder)
parcel_errors <- data.frame(Landcov = c(rep("UKCEH", 7), rep("CORINE", 7),
                                        rep("ODSE-LU", 7), rep("OSMLU", 7)),
                            Treecov = rep(c("TCD10m", "TCD100m", "ETH-CHM",
                                          "Meta-CHM", "TOW", "SWF",
                                          "DeepForest"), 4),
                            Omission = NA, mat_omission = NA, Comission = NA)

landcov_nums <-c(rep(4,7), rep(1,7), rep(2,7), rep(3,7))
treecov_nums <- rep(c(5,6,7,10,8,9,11), 4)
parcel_ag <- parcel_results2[which(parcel_results2$Type %in% 
                                     c("SA", "SP")),]
parcel_oth <- parcel_results2[which(parcel_results2$Type != "SA" &
                                      parcel_results2$Type != "SP"),]

for(i in 1:nrow(parcel_errors)){
  if(treecov_nums[i] != 8 & treecov_nums[i] != 7 & treecov_nums[i] != 10) {
    #omission
    parcel_errors[i,3] <- sum(parcel_ag$overlap_a[which(!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                      (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                    parcel_ag[,treecov_nums[i]] < 5)) ]) / sum(parcel_ag$overlap_a)
    #omission  mature
    parcel_errors[i,4] <- sum(parcel_ag$overlap_a[which((!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                                          (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                          parcel_ag[,treecov_nums[i]] < 5))  & parcel_ag$Planted < 2000)]) / sum(parcel_ag$overlap_a[parcel_ag$Planted < 2000])
    
    #comission
    parcel_errors[i,5] <- sum(parcel_oth$overlap_a[which(parcel_oth[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                           parcel_oth[,treecov_nums[i]] >= 5)]) / sum(parcel_oth$overlap_a)
    
  } else {
    if(treecov_nums[i] == 8){
      #special case for TOW only in england
      parcel_errors[i,3] <- sum(parcel_ag$overlap_a[which(!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                                            (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                            parcel_ag[,treecov_nums[i]] < 5) & parcel_ag$Country == "England") ]) / sum(parcel_ag$overlap_a[parcel_ag$Country == "England"])
      #omission  mature
      parcel_errors[i,4] <- sum(parcel_ag$overlap_a[which((!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                                             (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                             parcel_ag[,treecov_nums[i]] < 5))  & parcel_ag$Planted < 2000 & 
                                                      parcel_ag$Country == "England")]) / sum(parcel_ag$overlap_a[parcel_ag$Planted < 2000 & 
                                                                                                                    parcel_ag$Country == "England"])  
      #comission
      parcel_errors[i,5] <- sum(parcel_oth$overlap_a[which(parcel_oth[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                             parcel_oth[,treecov_nums[i]] >= 5 &
                                                             parcel_oth$Country == "England")]) /sum(parcel_oth$overlap_a[parcel_oth$Country == "England"])
      
      
    } else {
      #CHM need a 1m threshold not 5%
      parcel_errors[i,3] <- sum(parcel_ag$overlap_a[which(!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                                            (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                            parcel_ag[,treecov_nums[i]] < 1)) ]) / sum(parcel_ag$overlap_a)
      #omission  mature
      parcel_errors[i,4] <- sum(parcel_ag$overlap_a[which((!(parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable")) |
                                                             (parcel_ag[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                             parcel_ag[,treecov_nums[i]] < 1))  & parcel_ag$Planted < 2000)]) / sum(parcel_ag$overlap_a[parcel_ag$Planted < 2000])
      #omission
      parcel_errors[i,5] <- sum(parcel_oth$overlap_a[which(parcel_oth[,landcov_nums[i]] %in% c("Pasture", "Arable") &
                                                             parcel_oth[,treecov_nums[i]] >= 1)]) / sum(parcel_oth$overlap_a)
      
      
    }
 
  }
}
#simplyfing the parcel results
parcel_errors[,3:5] <- round(parcel_errors[,3:5], 3) *100








#do lc by full area and then treecover with buffer btw

#comparing areas
library(dplyr)

as.data.frame(studysites) %>% group_by(Name, Type) %>% summarise(Area = sum(area/10000))

parcel_results %>% group_by(Name, Type) %>% summarise(Area = sum(overlap_a/10000))
#mostly similar! some cutoffs but that is due to area I think
#note results will differ if I compare via studysite area vs not
#most differences are in the woodland control

#getting ggplot
parcel_results$ODSE2 <- factor(parcel_results$ODSE,
                               levels = c(1,8,9,15,16,17,
                                          18,19,21,27,28),
                               labels = c("Urban", "Urban", "Arable", 
                                          "Pasture", "Forest", "Forest", 
                                          "Grass", "Moore", "Forest", "Wetland",
                                          "Wetland"))
parcel_results$OSMLU2 <- factor(parcel_results$OSMLU,
                                levels = c(5,11, 12, 13, 14, 21,
                                           22, 23, 31, 32, 33, 41),
                                labels = c("Water", "Urban",
                                           "Industrial", "Industrial",
                                           "Artificial Vegetation",
                                           "Arable", "Orchard", "Pasture",
                                           "Forest", "Shrub", "Open", "Wetland"))
parcel_results$CORINE2 <- factor(parcel_results$CORINE,
                                levels = c(2,11,12,18,20,21,23,
                                           24,25,26,41),
                                labels = c("Urban", "Sport", "Arable",
                                           "Pasture", "Arable",
                                           "Arable", "Forest", "Forest", 
                                           "Forest", "Grass", "Water"))
ggplot(data = parcel_results[which(parcel_results$Planted < 2000),], 
       aes(x = Type, y = overlap_a, fill = ODSE2)) +
  geom_col(position = "fill")
#note only about a 12.5% error in the forst identificaiton here
ggplot(data = parcel_results, 
       aes(x = Type, y = overlap_a, fill = ODSE2)) +
  geom_col(position = "fill") #okay this is now shit
#loads of the WC are pasture...

ggplot(data = parcel_results, 
       aes(x = Type, fill = OSMLU2)) + geom_bar(position = "fill")

#checking CHM data
ggplot(data = parcel_results[which(parcel_results$Planted < 2000 |
                                     parcel_results$Type == "WC" |
                                     parcel_results$Type == "AC"),], 
       aes(x = Type, y = CHM)) +
  geom_violin() +
  geom_hline(yintercept = 5) 
