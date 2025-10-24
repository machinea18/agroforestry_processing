library(terra)
#n <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/Allerton.gpkg")
n <- sf::st_read("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/Allerton.gpkg")
files <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/",
                    full.names = T)
files <- sort(files)
files_RGB <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/",
                        full.names = T, pattern = "*tiff$")
files_RGB <- sort(files_RGB)
#note we have an extra file

#getting a data  frame
annotations <- data.frame(image_path = NA, xmin = NA, ymin = NA,
                          xmax = NA, ymax = NA, label = NA)

View(cbind(c(files, NA, NA), files_RGB)) # note Gibbson and whitehall have no trees
files <- c(files[1:8], NA, files[9:29], NA, files[30])
View(cbind(c(files), files_RGB))  
files <- files[-10]
View(cbind(c(files), files_RGB)) 
names <- sub(".*/", "", files_RGB)
names <- sub("(.*?)[\\.|:].*", "\\1", names)
for(i in 1:length(files)){
  if(!is.na(files[i])) {
    n <- vect(sf::st_read(files[i]))
    n <- na.omit(n, geom=TRUE)
    n$label <- "Tree"
    writeVector(n, paste("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes2/",
                         names[i], ".shp", sep = ""))
  }
}

#going from geoms to boxes now
annotations <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_temp.csv")
annotations_final <- data.frame(image_path = NA, xmin = NA, ymin = NA,
                          xmax = NA, ymax = NA, label = NA)
for(i in 1:nrow(annotations)) {
  annotations_final$image_path[i] <- annotations$image_path[i]
  annotations_final$label[i] <- annotations$label[i]
  pol <- sf::st_polygon(annotations$geometry[i])
}

#altering bounding boxes
annotations <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations3.csv")
for(i in 1:nrow(annotations)) {
  t_rast <- rast(paste("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/",
                       annotations$image_path[i], sep = ""))
  if(annotations$xmin[i] <0) {annotations$xmin[i] <- 0}
  if(annotations$ymin[i] <0) {annotations$ymin[i] <- 0}
  if(annotations$xmax[i] > ncol(t_rast)) {annotations$xmax[i] <- ncol(t_rast)}
  if(annotations$ymax[i] > nrow(t_rast)) {annotations$ymax[i] <- nrow(t_rast)}
}
  
write.csv(annotations,
          "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_file3.csv")

 for(i in 1:length(files)){
  if(!is.na(files[i])) {
    n <- sf::st_read(files[i])
    for(j in 1:length(sf::st_geometry(n))) {
      row <- annotations[1,]
      row$image_path <- files_RGB[i]
      row[2:5] <- sf::st_bbox(sf::st_geometry(n)[j])
      row$label <- "Tree"
      annotations <- rbind(annotations, row)
    }
  }
} 

#note that I have created an additional 6,367 annotations of trees across 32 images
annotations <- annotations[-1,]
#writing these
write.csv(annotations, file = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations.csv")

#reading annotations
annotations <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations.csv")
files_RGB <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training2/",
                        full.names = T, pattern = "*.tif$")
names <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training2/",
                    full.names = F, pattern = "*.tif$")
for(i in 1:length(files_RGB)) {
  temp <- rast(files_RGB[i])
  writeRaster(temp, paste("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/",
                          names[i], "f", sep = ""),
              datatype = "INT1U")
}

annotations$image_path <- sub(".*/", "", annotations$image_path)
write.csv(annotations, file = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_file.csv",
          row.names = FALSE)

#changing annotations to be relative to the image
library(terra)
annotations2 <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_file.csv")
#retaining only complete cases
annotations2 <- annotations2[complete.cases(annotations2),]
#removing everything with gibside
annotations2 <- annotations2[-which(annotations2$image_path == "Gibside.tiff"),]
annotations$inside <- NA
#note we are now left with only 6,142 annotations. Not bad though!
path <- "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/"
for(i in 1:nrow(annotations)){
  raster <- rast(paste(path, annotations$image_path[i], sep = ""))
  vector1 <- ext(unlist(annotations[i,c(2,4,3,5)]))
  annotations$inside[i] <- relate(raster, vector1, 
                                  relation = "covers")
  annotations$xmin[i] <- (annotations$xmin[i]-ext(raster)[1])*4
  annotations$xmax[i] <- (annotations$xmax[i]-ext(raster)[1])*4
  annotations$ymin[i] <- (annotations$ymin[i]-ext(raster)[3])*4
  annotations$ymax[i] <- (annotations$ymax[i]-ext(raster)[3])*4
  if(annotations$xmin[i] < 0) {annotations$xmin[i] <- 0}
  if(annotations$ymin[i] < 0) {annotations$ymin[i] <- 0}
  if(annotations$xmax[i] > ncol(raster)) {annotations$xmax[i] <- ncol(raster)}
  if(annotations$ymax[i] > nrow(raster)) {annotations$ymax[i] <- nrow(raster)}
}
#checking the times when something is not covered
View(annotations[which(annotations$inside == FALSE),])
annotations[6141:6142, 2:5] <- 0 #resetting 0 for no trees
write.csv(annotations[,1:6], file = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_file2.csv",
          row.names = FALSE)


###old stuff
#keeping the relevant ones
files_RGB_final <- files_RGB[c(1,6,11,12,13,14,19,20,21,22,28,
                               33,38,39,40,41,46,47,48,53,54,59,
                               60,65,66,67,68,73,78,83)]
original <- unique(annotations$image_path)
View(cbind(sort(original), sort(files_RGB_final)))
files_RGB_final <- c(files_RGB_final[1:11], 
                     "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/RGB_images/Henbant.tif",
                     files_RGB_final[12:30])
View(cbind(sort(original), sort(files_RGB_final)))
files_RGB_final <- c(files_RGB_final[1:31], 
                     "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/data/study_sites/RGB_images/Woodvalley.tif")
#all fixed! I have the corresponding ones
shapes <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes",
                     full.names = T, pattern = "*.gpkg$")
sort(shapes)
#removing whitehall and Gibbson as they have no trees
files_RGB_final <- sort(files_RGB_final)
original <- sort(original)
files_RGB_final <- c(files_RGB_final[1:8],files_RGB_final[10:30], files_RGB_final[32])
original <- c(original[1:8],original[10:30], original[32])
View(cbind(shapes, original))


#okay now we loop
library(terra)
library(sf)
for(i in 1:length(original)) {
  tif_file <- rast(files_RGB_final[i])
  shape_file <- st_read(shapes[i])
  tif_file <- crop(tif_file, st_bbox(shape_file))
  writeRaster(tif_file, original[i], overwrite = TRUE,
              datatype = "INT1U")
}
files <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/", 
                    full.names = TRUE, pattern = "*.tiff$")
t <- rast(files[1])
writeRaster(t, "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/test_Gibbson.tiff",
            datatype = "INT8U", overwrite = TRUE)
t <- rast("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/test_Gibbson.tiff")
t <- rast(files[2])
writeRaster(t, "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/test_Whitehall.tiff", 
            datatype = "INT1U")
#write.csv(annotations, file = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations.csv")


#okay now creating a crop
to_tile <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/", 
           full.names = TRUE, pattern = "*.tiff$")
names <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/", 
                    full.names = FALSE, pattern = "*.tiff$",
                    ignore.case = TRUE)
for(i in 1:length(to_tile)) {
  temp_rast <- rast(to_tile[i])
  makeTiles(temp_rast, y = 400, buffer = 20, 
            filename=paste("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training_cropped/",
                           names[i], sep = ""))
}

#setting up another annotations file
annotations <- read.csv("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations.csv")
annotations <- annotations[complete.cases(annotations),]
write.csv(annotations, 
          "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/annotations_file.csv")
tiles <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training_cropped/", 
                      full.names = TRUE, pattern = "*.tiff$")
trees_sf <- st_as_sf(annotations,
                     coords = c("xmin", "ymin", "xmax", "ymax"),
                     crs = crs(temp_rast))
trees_vect <- vect(trees_sf)
annotations_temp <- annotations[0,]

for(i in 1:length(tiles)) {
  temp_rast <- rast(tiles[i])
  n <- which(is.related(trees_vect, temp_rast, relation = 'intersects'))
  if(length(n) == 0) {
    temp_an <- annotations_temp[0,]
    temp_an[1,2:5] <- 0
    temp_an$image_path[1] <- tiles[i]
    temp_an$label[1] <- 'tree'
    annotations_temp <- rbind(annotations_temp, temp_an)
  } else {
    temp_an <- annotations_temp[0,]
    temp_an[1:length(n), 1:6] <- NA
    temp_an$image_path <- tiles[i]
    temp_an$label <- 'tree'
    for(j in 1:length(n)) {
      n_relevant <- n[j]
      temp_an[j,2:5] <- st_bbox(st_geometry(trees_sf[n_relevant,]))
    }
    annotations_temp <- rbind(annotations_temp, temp_an)
  }
}
