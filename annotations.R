library(terra)
#n <- vect("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/Allerton.gpkg")
n <- sf::st_read("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/Allerton.gpkg")
files <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/training_shapes/",
                    full.names = T)
files <- sort(files)
files_RGB <- list.files("C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/training_deepforest/RGB_training/",
                        full.names = T, pattern = "*tif$")
files_RGB <- sort(files_RGB)
#note we have an extra file

#getting a data  frame
annotations <- data.frame(image_path = NA, xmin = NA, ymin = NA,
                          xmax = NA, ymax = NA, label = NA)

View(cbind(c(files, NA, NA), files_RGB)) # note Gibbson and whitehall have no trees
files <- c(files[1:8], NA, files[9:29], NA, files[30])
View(cbind(c(files), files_RGB))  

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
