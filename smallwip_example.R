library(terra)
library(sf)
library(spatialEco)
library(lidR)
library(tidyterra)
library(dplyr)
library(whitebox)
library(MultiscaleDTM)
library(randomForest)
library(caret)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")

#################### Watershed & DEM ########################
hb <- vect("UplandWetlandGradient/data/hbef_wsheds/hbef_wsheds.shp")
hbdem <- rast("UplandWetlandGradient/data/dem1m.tif")
plot(hbdem)



#################### NWI Wetlands ########################
nwi <- vect("UplandWetlandGradient/data/HU8_01070001_Watershed/HU8_01070001_Wetlands.shp") |> project("EPSG:26919")
hbnwi <- nwi |> crop(ext(hbdem)) 
names(hbnwi)
plot(hbdem)
plot(hbnwi, "WETLAND_TY", type = "classes", add = T)

#################### Get training data - upland points #################### 

set.seed(11)
randupl <- spatSample(hbdem, 2000, as.points = TRUE, xy = TRUE)

hbnwi_buff <- buffer(hbnwi, 10) # buffer to remove potential wet areas

hbupl_pts <- terra::mask(randupl, hbnwi_buff, inverse = T) |> 
    mutate(class = "UPL") |>
    select(-dem1m)

plot(hbupl_pts, "class", type = "classes")
lines(hbnwi_buff)

#################### Get training data - wetland upland points #################### 

hbwet_pts <- spatSample(hbnwi, size = 500) |> 
    mutate(class = "WET") |>
    select(class)

plot(hbwet_pts, "class" ,type = "classes", col = "blue", cex = 0.2)
plot(hbupl_pts, "class", type = "classes", col = "red", cex = 0.2, add = T)

#################### Combine wetland and upland points #################### 

hbpts_all <- rbind(hbupl_pts, hbwet_pts) 
hbpts_ext <- terra::extract(hbdem, hbpts_all, bind = T) 
hbpts <- hbpts_ext |> dplyr::filter(!is.na(dem1m))
writeVector(hbpts, "UplandWetlandGradient/data/derived_data/hbpts_raw.gpkg", overwrite=TRUE)

plot(hbdem) 
plot(hbpts, "class", type = "classes", cex = 0.3, add = T)

#################### Make multiscale terrain metrics #################### 

# hbslp_3 <- SlpAsp(hbdem, w = c(3,3), metrics = "slope", filename = "UplandWetlandGradient/data/derived_data/hbslp_3.tif", overwrite = T)
# 
# hbslp_27 <- SlpAsp(hbdem, w = c(27,27), metrics = "slope", filename = "UplandWetlandGradient/data/derived_data/hbslp_27.tif", overwrite = T)
# 
# hbslp_81 <- SlpAsp(hbdem, w = c(81,81), metrics = "slope", filename = "UplandWetlandGradient/data/derived_data/hbslp_81.tif", overwrite = T)
# 
# hbtpi_3 <- TPI(hbdem, w = c(3,3), 
#                shape="rectangle", stand="none", na.rm = TRUE, 
#                filename = "UplandWetlandGradient/data/derived_data/hbtpi_3.tif", overwrite = T)
# 
# hbtpi_27 <- TPI(hbdem, w = c(27,27), 
#                 shape="rectangle", stand="none", na.rm = TRUE,
#                 filename = "UplandWetlandGradient/data/derived_data/hbtpi_27.tif", overwrite = T)
# 
# hbtpi_81 <- TPI(hbdem, w = c(81,81), 
#                 shape="rectangle", stand="none", na.rm = TRUE,
#                 filename = "UplandWetlandGradient/data/derived_data/hbtpi_81.tif", overwrite = T)
# 
# hbcurv_3 <- Qfit(hbdem, w = c(3,3), 
#                  metrics = c("meanc", "profc", "planc"), unit = "degrees", na.rm = TRUE,
#                  filename = "UplandWetlandGradient/data/derived_data/hbcurv_3.tif", overwrite = T)
# 
# hbcurv_27 <- Qfit(hbdem, w = c(27,27), 
#                   metrics = c("meanc", "profc", "planc"), unit = "degrees", na.rm = TRUE,
#                   filename = "UplandWetlandGradient/data/derived_data/hbcurv_27.tif", overwrite = T)
# 
# hbcurv_81 <- Qfit(hbdem, w = c(81,81), 
#                   metrics = c("meanc", "profc", "planc"), unit = "degrees", na.rm = TRUE,
#                   filename = "UplandWetlandGradient/data/derived_data/hbcurv_81.tif", overwrite = T)


#################### Multiscale terrain metrics already made #################### 

hbslp_3 <- rast("UplandWetlandGradient/data/derived_data/hbslp_3.tif")
hbslp_27<- rast("UplandWetlandGradient/data/derived_data/hbslp_27.tif")
hbslp_81<- rast("UplandWetlandGradient/data/derived_data/hbslp_81.tif")
hbcurv_3<- rast("UplandWetlandGradient/data/derived_data/hbcurv_3.tif")
hbcurv_27<- rast("UplandWetlandGradient/data/derived_data/hbcurv_27.tif")
hbcurv_81<- rast("UplandWetlandGradient/data/derived_data/hbcurv_81.tif")
hbtpi_3<- rast("UplandWetlandGradient/data/derived_data/hbtpi_3.tif")
hbtpi_27<- rast("UplandWetlandGradient/data/derived_data/hbtpi_27.tif")
hbtpi_81<- rast("UplandWetlandGradient/data/derived_data/hbtpi_81.tif")


#################### Stack multiscale terrain metrics and extract to training points #################### 

super_stack <- c(hbslp_3, hbslp_27, hbslp_81, 
                 hbcurv_3, hbcurv_27, hbcurv_81,
                 hbtpi_3, hbtpi_27, hbtpi_81)

hbpts_extract <- hbpts |> terra::extract(x = super_stack, bind = TRUE, xy = TRUE) |>
    drop_na()
names(hbpts_extract) <- (c("class", "dem", "slp_3", "slp_27", "slp_81", 
                           "meancurv_3", "prof_curv_3", "plan_curv_3",
                           "meancurv_27", "prof_curv_27", "plan_curv_27",
                           "meancurv_81", "prof_curv_81", "plan_curv_81",
                           "tpi_3", "tpi_27", "tpi_81", "x", "y"))
hbpts_extract$class <- as.factor(hbpts_extract$class)

#writeVector(hbpts_extract, "UplandWetlandGradient/data/derived_data/hbpts_hydrog_extract.gpkg", overwrite = TRUE)

#################### Split training points into training and testing #################### 

set.seed(11)

hbpts_exdf <- as.data.frame(hbpts_extract)

# Validation Set 
train.index <- as.vector(sample(c(1:nrow(hbpts_exdf)), 0.7*nrow(hbpts_exdf), replace=F))

train <- hbpts_exdf[train.index, c("class", "dem", "slp_3", "slp_27", "slp_81", 
                                   "meancurv_3", "prof_curv_3", "plan_curv_3",
                                   "meancurv_27", "prof_curv_27", "plan_curv_27",
                                   "meancurv_81", "prof_curv_81", "plan_curv_81",
                                   "tpi_3", "tpi_27", "tpi_81")]

test <- hbpts_exdf[-train.index, c("class", "dem", "slp_3", "slp_27", "slp_81", 
                                   "meancurv_3", "prof_curv_3", "plan_curv_3",
                                   "meancurv_27", "prof_curv_27", "plan_curv_27",
                                   "meancurv_81", "prof_curv_81", "plan_curv_81",
                                   "tpi_3", "tpi_27", "tpi_81")]

#################### Run random forest on training data #################### 

rf_model <- randomForest((class) ~ .,
                         ntree = 500, na.action = na.omit,
                         importance = TRUE, data = train)
plot(rf_model)

rf.test <- predict(rf_model, newdata = test, type = "response")
rf.train <- predict(rf_model, newdata = train)
caret::confusionMatrix(rf.train, train$class)
caret::confusionMatrix(rf.test, test$class)
vip::vip(rf_model)


#################### Adjust variable names and predict random forest to raster stack #################### 

pred_stack <- c(hbdem, super_stack)


names(pred_stack) <- (c("dem", "slp_3", "slp_27", "slp_81", 
                        "meancurv_3", "prof_curv_3", "plan_curv_3",
                        "meancurv_27", "prof_curv_27", "plan_curv_27",
                        "meancurv_81", "prof_curv_81", "plan_curv_81",
                        "tpi_3", "tpi_27", "tpi_81"))

#hbwip <- predict(pred_stack, rf_model, type = "prob", filename = "UplandWetlandGradient/data/derived_data/hbwip_hydrog.tif", overwrite = TRUE)

hbwip <- rast("UplandWetlandGradient/data/derived_data/hbwip.tif")
plot(hbwip$WET)
