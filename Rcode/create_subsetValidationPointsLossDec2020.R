library(sf)
library(dplyr)
library(raster)


F1 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary11.shp')
F2 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary12.shp')
F3 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary13.shp')
F4 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary14.shp')
F5 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary15.shp')
F6 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary16.shp')
F7 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary17.shp')
F8 <- st_read('Data/SHP/LossD2020/FI_Loss_NAtureCommentary18.shp')


# st_crs(S1) = NA

# write_sf(F11,'Data/SHP/F11.shp')

#### shuffle
set.seed(42)
# Next, you use the sample()function to shuffle the row indices of the dataframe(df). You can later use these indices to reorder the dataset.
rows1 <- sample(nrow(F1))
rows2 <- sample(nrow(F2))
rows3 <- sample(nrow(F3))
rows4 <- sample(nrow(F4))
rows5 <- sample(nrow(F5))
rows6 <- sample(nrow(F6))
rows7 <- sample(nrow(F7))
rows8 <- sample(nrow(F8))

F1 <- F1[rows1, ]
F2 <- F2[rows2, ]
F3 <- F3[rows3, ]
F4 <- F4[rows4, ]
F5 <- F5[rows5, ]
F6 <- F6[rows6, ]
F7 <- F7[rows7, ]
F8 <- F8[rows8, ]

TOTF <- rbind(F1[1:200,],F2[1:200,],F3[1:200,],F4[1:200,],F5[1:200,],F6[1:200,],F7[1:200,],F8[1:200,])
TOTF$Year <- rep(seq(11,18),each=200)

TOTF$Country <- 'Finland'
# FirstSample <- sample_n(TOT,20000)
TOTF$ID <- seq(1,nrow(TOTF))
TOTF$CLASS <- rep(0,nrow(TOTF))
TOTF$Date <- rep(0,nrow(TOTF))
TOTF$FLAG <- rep(0,nrow(TOTF))
TOTF <- TOTF[, c("ID",'Year','CLASS','Date','Country','FLAG')]

# write_sf(TOTF,'Data/SHP/LossD2020/TOTFinland.shp')

TOTFinland <- st_read('Data/SHP/LossD2020/TOTFinland.shp')



rm(F1,F2,F3,F4,F5,F6,F7,F8)

S1 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary11S.shp')
S2 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary12S.shp')
S3 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary13S.shp')
S4 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary14S.shp')
S5 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary15S.shp')
S6 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary16S.shp')
S7 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary17S.shp')
S8 <- st_read('Data/SHP/LossD2020/SW_Loss_NAtureCommentary18S.shp')



rows1 <- sample(nrow(S1))
rows2 <- sample(nrow(S2))
rows3 <- sample(nrow(S3))
rows4 <- sample(nrow(S4))
rows5 <- sample(nrow(S5))
rows6 <- sample(nrow(S6))
rows7 <- sample(nrow(S7))
rows8 <- sample(nrow(S8))

S1 <- S1[rows1, ]
S2 <- S2[rows2, ]
S3 <- S3[rows3, ]
S4 <- S4[rows4, ]
S5 <- S5[rows5, ]
S6 <- S6[rows6, ]
S7 <- S7[rows7, ]
SF8 <- S8[rows8, ]

TOTS <- rbind(S1[1:200,],S2[1:200,],S3[1:200,],S4[1:200,],S5[1:200,],S6[1:200,],S7[1:200,],S8[1:200,])
# st_crs(TOTS) = NA

TOTS$Year <- rep(seq(11,18),each=200)

TOTS$Country <- 'Sweden'
# FirstSample <- sample_n(TOT,20000)
TOTS$ID <- seq(1,nrow(TOTS))
TOTS$CLASS <- rep(0,nrow(TOTS))
TOTS$Date <- rep(0,nrow(TOTS))
TOTS$FLAG <- rep(0,nrow(TOTS))
TOTS <- TOTS[, c("ID",'Year','CLASS','Date','Country','FLAG')]

# write_sf(TOTS,'Data/SHP/LossD2020/TOTSweden.shp')
TOTS <- st_read('Data/SHP/LossD2020/TOTSweden.shp')




TOT <- rbind(TOTFinland,TOTS)


# write_sf(ggg1000_17,'/data/cecchgu/Forest_management_EU/ValidationS1/SHP_patches/merged_17_1000.shp')
# write_sf(ggg1000,'/data/cecchgu/Forest_management_EU/ValidationS1/SHP_patches/merged_12_1000.shp')




# TOT$Date2 <- rep(0,nrow(TOT))

####### filter big patches


rows <- sample(nrow(TOT))

# Finally, you can use this random vector to reorder the diamonds dataset:
TOT <- TOT[rows, ]

TOT$ID <- seq(1,3200)



TOT <- st_transform(TOT, 3857)
TOT <- st_buffer(TOT, 30,endCapStyle= "SQUARE")

# write_sf(TOT,'Data/SHP/LossD2020/TOTSwedenFinland.shp')


TOT1 <- TOT %>% filter(ID<= 1100)
TOT2 <- TOT %>% filter(ID> 1100 & ID <= 2200)
TOT3 <- TOT %>% filter(ID> 2200 & ID <= 3200)


# FirstSample2<- FirstSample %>% filter(ID> 10000)

write_sf(TOT1,'Data/SHP/LossD2020/TOT1.shp')
write_sf(TOT2,'Data/SHP/LossD2020/TOT2.shp')
write_sf(TOT3,'Data/SHP/LossD2020/TOT3.shp')




# F1r <- st_transform(F1[1:7000,], 3857)
# F1r <- st_buffer(F1r, 30,endCapStyle= "SQUARE")
# write_sf(F1r,'Data/SHP/LossD2020/F1r.shp')
