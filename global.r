library(dplyr)
library(sf)

lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    
    
    as.character(as.expression(eq));
}

data <- read.csv("data/CompiledData-Table 1.csv")
#colnames(data)[23] <- "latitude"
#colnames(data)[22] <- "longitude"
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data$ModernArch.[data$ModernArch.=="archaeological"] <- "Archaeological"
data$ModernArch.[data$ModernArch.=="modern"] <- "Modern"
data$TissueTType[data$TissueTType=="Bone, tooth"] <- "Bone or tooth"
data$TissueTType[data$TissueTType==" Bone"] <- "Bone"
data$TissueTType[data$TissueTType=="Bone "] <- "Bone"
data$TissueTType[data$TissueTType=="cob"] <- "Cob"
data$TissueTType[data$TissueTType=="Corn?"] <- "Corn"
data$TissueTType[data$TissueTType=="grass"] <- "Grass"
data$TissueTType[data$TissueTType=="seed"] <- "Seed"
data$TissueTType[data$TissueTType=="stalk"] <- "Stalk"
data$TissueTType[data$TissueTType=="tuber"] <- "Tuber"
data$TissueTType[data$TissueTType=="grass"] <- "Grass"
data$TissueTType[data$TissueTType=="leaf"] <- "LEaf"
data$TissueTType[data$TissueTType=="hip"] <- "Hip"
data$TissueTType[data$TissueTType=="nut"] <- "Nut"
data$TissueTType[data$TissueTType=="kernel"] <- "Kernel"
data$TissueTType[data$TissueTType=="rhizome"] <- "Rhizome"
data$IsotopeSystem.s.[data$IsotopeSystem.s.=="C "] <- "C"
data$IsotopeSystem.s.[data$IsotopeSystem.s.=="C, O "] <- "C, O"

data_convert <- data[is.na(data$Latitude),]
data <- data[!is.na(data$Latitude),]

data_convert$UTM_Zone <- as.numeric(gsub("S", "", data_convert$UTM))
data_convert_12 <- data_convert[data_convert$UTM_Zone %in% 12,]
data_convert_13 <- data_convert[!data_convert$UTM_Zone %in% 12,]
data_convert_13 <- data_convert_13[!is.na(data_convert_13$Easting),]

utmcoor<- st_as_sf(data_convert_12, coords = c("Easting", "Northing"), crs = paste0("+proj=utm +zone=12"))
#zone= UTM zone
# converting
longlatcoor <- st_transform(utmcoor, crs = 4326)  # EPSG:4326 is the WGS84 coordinate system
coords <- st_coordinates(longlatcoor)
data_convert_12$Longitude <- coords[,1]
data_convert_12$Latitude <- coords[,2]

utmcoor<- st_as_sf(data_convert_13, coords = c("Easting", "Northing"), crs = paste0("+proj=utm +zone=13"))
#zone= UTM zone
# converting
longlatcoor <- st_transform(utmcoor, crs = 4326)  # EPSG:4326 is the WGS84 coordinate system
coords <- st_coordinates(longlatcoor)
data_convert_13$Longitude <- coords[,1]
data_convert_13$Latitude <- coords[,2]

data <- as.data.frame(data.table::rbindlist(list(data, data_convert_12, data_convert_13), fill=T, use.names=T))



