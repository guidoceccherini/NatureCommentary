library(sf)
library(dplyr)
library(raster)


F1 <- st_read('Data/SHP/FI_NAtureCommentary.shp')
F2 <- st_read('Data/SHP/SW_NAtureCommentary.shp')

F1$country <- 'Finland'
F2$country <- 'Sweden'

TOTp <- rbind(F1,F2)
TOTp$ID <- seq(1,nrow(TOTp))
TOTp$CLASS <- rep(0,nrow(TOTp))
TOTp$Date <- rep(0,nrow(TOTp))

TOTp <- TOTp[, c("ID",'country','CLASS','Date')]
write_sf(TOTp,'Data/SHP/Buffered/TOT_ForestPoints.shp')


F1 <- st_transform(F1, 3857)
F1 <- st_buffer(F1, 30,endCapStyle= "SQUARE")
F2 <- st_transform(F2, 3857)
F2 <- st_buffer(F2, 30,endCapStyle= "SQUARE")
# write_sf(F11,'Data/SHP/F11.shp')


TOT <- rbind(F1,F2)

# FirstSample <- sample_n(TOT,20000)


# write_sf(ggg1000_17,'/data/cecchgu/Forest_management_EU/ValidationS1/SHP_patches/merged_17_1000.shp')
# write_sf(ggg1000,'/data/cecchgu/Forest_management_EU/ValidationS1/SHP_patches/merged_12_1000.shp')


TOT$ID <- seq(1,nrow(TOT))
TOT$CLASS <- rep(0,nrow(TOT))
TOT$Date1 <- rep(0,nrow(TOT))
TOT$Date2 <- rep(0,nrow(TOT))

TOT <- TOT[, c("ID",'country','CLASS','Date1','Date2')]
####### filter big patches


TOT1 <- TOT %>% filter(ID<= 1000)
TOT2 <- TOT %>% filter(ID> 1000 & ID <= 2000)
TOT3 <- TOT %>% filter(ID> 2000 & ID <= 3000)
TOT4 <- TOT %>% filter(ID> 3000 & ID <= 4000)
TOT5 <- TOT %>% filter(ID> 4000 & ID <= 5000)
TOT6 <- TOT %>% filter(ID> 5000 & ID <= 6000)
TOT7 <- TOT %>% filter(ID> 6000 & ID <= 7000)
TOT8 <- TOT %>% filter(ID> 7000 & ID <= 8000)
TOT9 <- TOT %>% filter(ID> 8000 & ID <= 9000)
TOT10 <- TOT %>% filter(ID> 9000 & ID <= 10000)
TOT11 <- TOT %>% filter(ID> 10000 )

# FirstSample2<- FirstSample %>% filter(ID> 10000)
write_sf(TOT,'Data/SHP/Buffered/TOT_Forest.shp')

write_sf(TOT1,'Data/SHP/Buffered/TOT1.shp')
write_sf(TOT2,'Data/SHP/Buffered/TOT2.shp')
write_sf(TOT3,'Data/SHP/Buffered/TOT3.shp')
write_sf(TOT4,'Data/SHP/Buffered/TOT4.shp')
write_sf(TOT5,'Data/SHP/Buffered/TOT5.shp')
write_sf(TOT6,'Data/SHP/Buffered/TOT6.shp')
write_sf(TOT7,'Data/SHP/Buffered/TOT7.shp')
write_sf(TOT8,'Data/SHP/Buffered/TOT8.shp')
write_sf(TOT9,'Data/SHP/TOT9.shp')
write_sf(TOT10,'Data/SHP/Buffered/TOT10.shp')
write_sf(TOT11,'Data/SHP/Buffered/TOT11.shp')
