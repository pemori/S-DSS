### Land-Use suitability modelling in Puchuncavi-Quintero based on the As distribution in soils ###

### Edited by Andrés Salazar (GeoStyle), Pedro Mondaca, Ignacio Nuñez-Hidalgo
### Submitted in Land Degradation & Development

### 0. Clean workspace ###

rm(list = ls())
graphics.off()
gc()

### 1. Load packages ###
 ## # Checks and installs the package if not already installed ##
  ## Run twice to install and load##
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(raster)) {install.packages("raster")}
if (!require(sf)) {install.packages("sf")}
if (!require(mapview)) {install.packages("mapview")}
if (!require(mapedit)) {install.packages("mapedit")}
if (!require(sp)) {install.packages("sp")}
if (!require(landscapemetrics)) {install.packages("landscapemetrics")}
if (!require(rio)) {install.packages("rio")}


### 2. Data loading and pre-processing ###

setwd("C:/futuro") # Set working directory
input_folder  <- paste0(getwd(),"/input")
output_folder <- paste0(getwd(),"/output")


k <- raster("input/krig.tif") #Load Kriging raster
ks <- raster("input/krig_s.tif") #Load Kriging-s raster
ae <- read_sf("input/Area_est_PROCD.shp") %>% st_zm() #Load study area shapefile
k_ae <- k %>% crop(ae) %>% mask(ae) #Crop and Mask kriging to study area shapefile

### 3. Raster reclassification ###
## Defines a function 'fu' that operates on raster data based on parameter 'emi' ##
fu <- function(emi){
  
  map(c(5,10,20),function(year){ # Loops through three different 'year' values.
  suma <- k %>% values %>% na.omit %>% sum # Calculates the sum of 'k' values after removing NAs
  prop <- k * 100 /suma # Calculates 'prop' as a percentage of 'k' values.
  
  
  ## Unit conversions
  p_to <- prop * emi * year# Converts 'ton/ha' to 'g/m2'.
  p_to2 <- (p_to/180)*1000 # Converts 'g/m2' to 'mg/kg'.
  p_to3 <- p_to2 + ks # Adds 'ks' (basal As concentration)
  
 ## Reclassification ##
  
  # Reclassifies 'p_to3' based on matrix 'm1', which contains the thresholds for residential land use #
  m1 <- c(0, 16.5, 1,
          16.5, 33, 2,
         33, 99999999, 3)
  
  mat1 <- matrix(m1, ncol = 3, byrow = T)
  reclass1 <- reclassify(p_to3, mat1)
  writeRaster(reclass1, paste("output/raster/","resid_",emi,"_", year,".tif", sep=""))
  
  # Similar operations are repeated for 'm2', which contains the thresholds for productive land use #
  m2 <- c(0, 80, 1,
          80, 160, 2,
          160, 999999999, 3)
  
  mat2 <- matrix(m2, ncol = 3, byrow = T)
  reclass2 <- reclassify(p_to3, mat2)
  writeRaster(reclass2, paste("output/raster/","equip_",emi,"_", year,".tif", sep=""))
  
  # Similar operations are repeated for 'm3', which contains the thresholds for green areas land use #
  m3 <- c(0, 46, 1,
          46, 93, 2,
          93, 9999999, 3)
  
  mat3 <- matrix(m3, ncol = 3, byrow = T)
  
  reclass3 <- reclassify(p_to3, mat3)
  
  writeRaster(reclass3, paste("output/raster/","esp_pub_",emi,"_", year,".tif", sep=""))
  })
  
}

# Calls the 'fu' function for different 'emi' values.
map(c(35,40,48),
      fu)



### 4. Graphication workbooks ###

# 1.1 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Defines 'm1' matrix for reclassification criteria
# Creates a matrix based on 'm1'
# Reclassifies the 'k' raster based on 'mat1'
m1 <- c(0, 16.5, 1,
        16.5, 33, 2,
        33, 99999999, 3)
mat1 <- matrix(m1, ncol = 3, byrow = T)
k_resid <- reclassify(k, mat1)

# Defines 'm2' matrix for reclassification criteria
# Creates a matrix based on 'm2'
# Reclassifies the 'k' raster based on 'mat2'
m2 <- c(0, 80, 1,
        80, 160, 2,
        160, 999999999, 3)
mat2 <- matrix(m2, ncol = 3, byrow = T)
k_equip <- reclassify(k, mat2)

# Defines 'm3' matrix for reclassification criteria
# Creates a matrix based on 'm3'
# Reclassifies the 'k' raster based on 'mat3
m3 <- c(0, 46, 1,
        46, 93, 2,
        93, 9999999, 3)
mat3 <- matrix(m3, ncol = 3, byrow = T)
k_esp <- reclassify(k, mat3)

### 1.1 compilation ###

# Combines the reclassified rasters into a list
k_rec <- list(k_resid, k_equip, k_esp)

 ## Performs operations on the 'k_rec' list to calculate landscape metrics and create a data frame
  ## Also format data.frame 
ak <- k_rec %>% 
  map(lsm_c_ca) %>% 
  reduce(bind_rows) %>% 
  dplyr::select(3,6) %>% 
  rename(clase=class, area=value) %>% 
  mutate(uso = c(rep("resid",3), 
                 rep("equip",2),
                 rep("esp",3))) %>% 
  pivot_wider(names_from = uso,
              values_from = area)

# Exports the data frame as an Excel file.
rio::export(ak,"output/excel/cuadro_1.1.xlsx")

# 1.2 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reclassifies the 'k_ae' raster based on the different land uses

kae_resid <- reclassify(k_ae, mat1)
kae_equip <- reclassify(k_ae, mat2)
kae_esp <-   reclassify(k_ae, mat3)

# Combines the reclassified 'k_ae' rasters into a list
kae_rec <-   list(kae_resid, kae_equip, kae_esp)

  ## Performs operations on the 'kae_rec' list to calculate landscape metrics and create a data frame
    ## Also format data.frame 
akae <- kae_rec %>% 
  map(lsm_c_ca) %>% 
  reduce(bind_rows) %>% 
  dplyr::select(3,6) %>% 
  rename(clase=class, area=value) %>% 
  mutate(uso = c(rep("resid",2), 
                 rep("equip",2),
                 rep("esp",3))) %>% 
  pivot_wider(names_from = uso,
              values_from = area) %>% 
  arrange(clase)
  
# Exports the data frame as an Excel file
rio::export(akae,"output/excel/cuadro_1.2.xlsx")

# 2.1 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Retrieves the names of TIFF files in the "output/raster" folder
nam <- list.files(path = "output/raster", full.names = F, pattern = "tif") %>% 
  str_remove_all(".tif") %>% # Removes the ".tif" extension from the file names
  str_replace("esp_pub", "esp") # Replaces "esp_pub" with "esp" in the file names

# Retrieves full paths to TIFF files adn reads the TIFF files as raster datasets
rast <- list.files(path = "output/raster", full.names = T, pattern = "tif") %>% 
  map(raster)

  ## Performs operations on the raster datasets to calculate landscape metrics and create a data frame
    ## Also format data.frame 
atp <- rast %>% 
  map(lsm_c_ca) %>%
  map2(nam,~mutate(.x, caso = .y)) %>% 
  reduce(bind_rows) %>% 
  dplyr::select(3,6,7) %>% 
  rename(clase=class, area=value) %>% 
  separate(caso, into = c("uso","as_emit","n_dep")) %>% 
  pivot_wider(names_from = uso, values_from = area) %>% 
  dplyr::select(as_emit, n_dep,clase, resid, equip, esp) %>% 
  mutate(n_dep = n_dep %>% as.numeric) %>% 
  arrange(as_emit,n_dep,clase)

# Exports the data frame as an Excel file
rio::export(atp,"output/excel/cuadro_2.1.xlsx")

# 2.2 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Retrieves full paths to TIFF files, read TIFF files as raster datasets, then crop them into study area
# And mask the cropped rasters using the study area
rast2 <- list.files(path = "output/raster", full.names = T, pattern = "tif") %>% 
  map(raster) %>%  
  map(~crop(.x, ae)) %>%  
  map(~mask(.x, ae))

  ## Performs operations on the raster datasets to calculate landscape metrics and create a data frame
    ## Also format data.frame
atp2 <- rast2 %>% 
  map(lsm_c_ca) %>%
  map2(nam,~mutate(.x, caso = .y)) %>% 
  reduce(bind_rows) %>% 
  dplyr::select(3,6,7) %>% 
  rename(clase=class, area=value) %>% 
  separate(caso, into = c("uso","as_emit","n_dep")) %>% 
  pivot_wider(names_from = uso, values_from = area) %>% 
  dplyr::select(as_emit, n_dep,clase, resid, equip, esp) %>% 
  mutate(n_dep = n_dep %>% as.numeric) %>% 
  arrange(as_emit,n_dep,clase)

# Exports the data frame as an Excel file
rio::export(atp2,"output/excel/cuadro_2.2.xlsx")


### END OF SCRIPT ###
