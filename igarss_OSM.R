library(terra)
points_all2 <- vect("study_sites/pall_processed_THESIS.gpkg")
#making loughgall UKCEH for OSM stuff
pall_dat <- as.data.frame(points_all2)
pall_dat2 <- as.data.frame(points_all2)
pall_dat[which(pall_dat$Name == "Loughgall"),28:38] <- pall_dat[which(pall_dat$Name == "Loughgall"),16:26]

pall_dat$UKCEH <- factor(pall_dat$UKCEH, levels = c(1, 12, 2, 21, 3, 4, 5),
                            labels = c("Woodland", "Rock", "Woodland", "Suburban", 
                                       "Arable", "Pasture", "Grassland"))
pall_dat$CORINE <- factor(pall_dat$CORINE, 
                             labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                        "Arable", "Trees", "Trees", "Trees", "Grass",
                                        "Moores", "Water"),
                             levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
pall_dat$UKCEH_CORINE <- factor(pall_dat$UKCEH_CORINE, 
                                   labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                              "Arable", "Trees", "Trees", "Trees", "Grass",
                                              "Moores", "Water"),
                                   levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
pall_dat$OSM_CORINE <- factor(pall_dat$OSM_CORINE, 
                                 labels = c("Urban", "Mines", "Sport", "Arable", "Pasture",
                                            "Arable", "Trees", "Trees", "Trees", "Grass",
                                            "Moores", "Water"),
                                 levels = c(2,7,11,12,18,20,23,24,25,26,27,41))
#ODSE
pall_dat$ODSE <- factor(pall_dat$ODSE, 
                           levels = c(1,8,9,15,16,17,18,19,21,28),
                           labels = c("Urban", "Arable", "Arable",
                                      "Pasture", "Trees", "Trees", "Grass",
                                      "Moores", "Trees", "Snow"))
pall_dat$UKCEH_ODSE <- factor(pall_dat$UKCEH_ODSE, 
                                 levels = c(1,8,9,15,16,17,18,19,21,28),
                                 labels = c("Urban", "Arable", "Arable",
                                            "Pasture", "Trees", "Trees", "Grass",
                                            "Moores", "Trees", "Snow"))
pall_dat$OSM_ODSE <- factor(pall_dat$OSM_ODSE, 
                               levels = c(1,8,9,15,16,17,18,19,21,28),
                               labels = c("Urban", "Arable", "Arable",
                                          "Pasture", "Trees", "Trees", "Grass",
                                          "Moores", "Trees", "Snow"))
#OSMLU
pall_dat$OSMLU <- factor(pall_dat$OSMLU, 
                            levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                            labels = c("Water", "Urban", "Industrial", "Mines",
                                       "Vegetation", "Arable", "Orchards", 
                                       "Pasture", "Trees", "Vegetation", "Bare",
                                       "Wetlands"))
pall_dat$UKCEH_OSMLU <- factor(pall_dat$UKCEH_OSMLU, 
                                  levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                                  labels = c("Water", "Urban", "Industrial", "Mines",
                                             "Vegetation", "Arable", "Orchards", 
                                             "Pasture", "Trees", "Vegetation", "Bare",
                                             "Wetlands"))
pall_dat$OSM_OSMLU <- factor(pall_dat$OSM_OSMLU, 
                                levels = c(5,11,12,13,14,21,22,23,31,32,33,41),
                                labels = c("Water", "Urban", "Industrial", "Mines",
                                           "Vegetation", "Arable", "Orchards", 
                                           "Pasture", "Trees", "Vegetation", "Bare",
                                           "Wetlands"))

#plotting these out
library(ggplot2)
library(ggpubr)
library(shadowtext)
UKCEH_cols <- c("#0C6805", "#D3D0FD", "#828282", "#642722",
                "#e6e64d", "#09FA0B")
CORINE_cols <- c("white", "black", "#ffe6ff", "#642722", "#e6e64d", "#0C6805",
                 "#09FA0B", "#6ea3d5", "#80f2e6")

ODSE_cols <- c("white", "#642722", "#e6e64d", "#0C6805", "#09FA0B", "#6ea3d5", "grey")
  


OSMLU_cols <- c("#80f2e6", "white", "black", "black", "#09FA0B", "#642722", "#0C8000",
                "#e6e64d", "#0C6805", "pink", "#6ea3d5")

  #for IGARSS conference
  B.1 <- ggplot(data = pall_dat, aes(fill = UKCEH, x = Type)) +
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
  B.2 <- ggplot(data = pall_dat, aes(fill = OSM_CORINE, x = Type)) +
    geom_bar(position = "fill") +
    theme_minimal()+
    guides(fill=guide_legend(nrow=2)) +
    geom_shadowtext(stat='count', aes(label=..count..), position = position_fill(vjust = .5), size = 3.5) +
    scale_fill_manual(name = "UKCEH", values = CORINE_cols, drop = FALSE) +
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
  B.3 <- ggplot(data = pall_dat, aes(fill = OSM_ODSE, x = Type)) +
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
  B.8 <- ggplot(data = pall_dat, aes(fill = OSM_OSMLU, x = Type)) +
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
  ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/conferences/igarss/landcovers_OSM.png", 
         units = "mm", width =280, height = 280, dpi = 900)
  
  #now processing, focussing on getting 
  #correcting this
  pall_dat$Meta_CHM <- pall_dat$Meta_CHM*100
  pall_dat$ETH <- pall_dat$ETH*100
  
  
  pGB_dat <-pall_dat[which(pall_dat$Name != "Loughgall"),]
  pE_dat <-pall_dat[which(pall_dat$Name != "Loughgall" &
                            pall_dat$Name != "Glensaugh" &
                            pall_dat$Name != "Parkhill" &
                            pall_dat$Name != "Henfaes"),]
  trees <- c(rep(c(12,13,12,13,12,13, 14,15),4), #non parcels
             rep(c(20,21,26,27,25,24,22,23),4),
             rep(c(31,32,37,38,36,35,33,34),4))
  lands <- c(rep(16, 8), rep(9, 8), rep(10, 8), rep(11, 8), #non parcels (ignore UKCEH)
             rep(16, 8), rep(17, 8), rep(18, 8), rep(19, 8), #UKCEH parcels
             rep(18, 8), rep(28,8), rep(29,8), rep(30,8)) #OSM ignore UKCEH

  points_all_results <- data.frame(Parcel=rep(NA,8*4*3),Landcov = rep(NA, 8*4*3), 
                                   Treecov=NA, AF_O = NA, AF_mat_O = NA,
                                   AF_C = NA)
  points_all_results$Parcel <- c(rep(NA,4*8), rep("UKCEH", 4*8),
                                 rep("OSM", 4*8))
  points_all_results$Landcov <- rep(c(rep("UKCEH", 8), rep("CORINE", 8),
                                      rep("ODSE", 8), rep("OSMLU", 8)),3)
  points_all_results$Treecov <- rep(c("TCD10m", "TCD100m", "SWF", "DF25",
                                      "Bluesky", "TOW", "ETH", "CHM"), 4)
  
  
  
  for(i in 1:length(trees)){
    if(points_all_results$Treecov[i] == "Bluesky") {
      AF_dat <- pGB_dat[which(pGB_dat$Type == "SA" |
                                 pGB_dat$Type == "SP"), c(lands[i], trees[i])]
      #AF_dat <- AF_dat[complete.cases(AF_dat),]
      points_all_results[i,4] <- (length(which((AF_dat[,1] != "Pasture" &
                                                  AF_dat[,1] != "Arable")| 
                                                 AF_dat[,2] < 5))/nrow(AF_dat))*100
      AF2_dat <- pGB_dat[which(pGB_dat$Type == "AC" |
                                  pGB_dat$Type == "WC"), c(lands[i], trees[i])]
      #AF2_dat <- AF2_dat[complete.cases(AF2_dat),]
      points_all_results[i,6] <- (length(which((AF2_dat[,1] == "Pasture" |
                                                  AF2_dat[,1] == "Arable")& 
                                                 AF2_dat[,2] > 5))/nrow(AF2_dat))*100
      AF_dat <- pGB_dat[which((pGB_dat$Type == "SA" |
                                  pGB_dat$Type == "SP") &
                                 pGB_dat$Planted<2000), c(lands[i], trees[i])]
      #AF_dat <- AF_dat[complete.cases(AF_dat),]
      points_all_results[i,5] <- (length(which((AF_dat[,1] != "Pasture" &
                                                  AF_dat[,1] != "Arable")| 
                                                 AF_dat[,2] < 5))/nrow(AF_dat))*100
    } else {
      if(points_all_results$Treecov[i] == "TOW") {
        AF_dat <- pE_dat[which(pE_dat$Type == "SA" |
                                   pE_dat$Type == "SP"), unlist(c(lands[i], trees[i]))]
        #AF_dat <- AF_dat[complete.cases(AF_dat),]
        points_all_results[i,4] <- (length(which((AF_dat[,1] != "Pasture" &
                                                    AF_dat[,1] != "Arable")| 
                                                   AF_dat[,2] < 5))/nrow(AF_dat))*100
        AF2_dat <- pE_dat[which(pE_dat$Type == "AC" |
                                    pE_dat$Type == "WC"), c(lands[i], trees[i])]
        #AF2_dat <- AF2_dat[complete.cases(AF2_dat),]
        points_all_results[i,6] <- (length(which((AF2_dat[,1] == "Pasture" |
                                                    AF2_dat[,1] == "Arable")& 
                                                   AF2_dat[,2] > 5))/nrow(AF2_dat))*100
        AF_dat <- pE_dat[which((pE_dat$Type == "SA" |
                                    pE_dat$Type == "SP") &
                                   pE_dat$Planted<2000), c(lands[i], trees[i])]
        #AF_dat <- AF_dat[complete.cases(AF_dat),]
        points_all_results[i,5] <- (length(which((AF_dat[,1] != "Pasture" &
                                                    AF_dat[,1] != "Arable")| 
                                                   AF_dat[,2] < 5))/nrow(AF_dat))*100        
      } else {
       AF_dat <- pall_dat[which(pall_dat$Type == "SA" |
                                  pall_dat$Type == "SP"), c(lands[i], trees[i])]
       #AF_dat <- AF_dat[complete.cases(AF_dat),]
       points_all_results[i,4] <- (length(which((AF_dat[,1] != "Pasture" &
                                           AF_dat[,1] != "Arable")| 
                                          AF_dat[,2] < 5))/nrow(AF_dat))*100
       AF2_dat <- pall_dat[which(pall_dat$Type == "AC" |
                                  pall_dat$Type == "WC"), c(lands[i], trees[i])]
       #AF2_dat <- AF2_dat[complete.cases(AF2_dat),]
       points_all_results[i,6] <- (length(which((AF2_dat[,1] == "Pasture" |
                                                   AF2_dat[,1] == "Arable")& 
                                                  AF2_dat[,2] > 5))/nrow(AF2_dat))*100
       AF_dat <- pall_dat[which((pall_dat$Type == "SA" |
                                  pall_dat$Type == "SP") &
                                  pall_dat$Planted<2000), c(lands[i], trees[i])]
       #AF_dat <- AF_dat[complete.cases(AF_dat),]
       points_all_results[i,5] <- (length(which((AF_dat[,1] != "Pasture" &
                                                   AF_dat[,1] != "Arable")| 
                                                  AF_dat[,2] < 5))/nrow(AF_dat))*100
      }
    }
  }
  view <- c(9,10,15:18,23:26, 31:32,65:96)  
 View(points_all_results[view,c(4:6)]) 
 
 