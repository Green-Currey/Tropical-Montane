
source('~/R/startup.R') 
#d:/projects/tropical N cycling Gay 2021/data



pctNpoints <- read_csv('15NDataSummarized.csv')
LER <- raster('Raster/globalLER23_5_5km.tif')
forest <- raster('Raster/TC/trop_tc_10.tif')


xy.N <- pctNpoints %>% dplyr::select(Longitude, Latitude)

# coordinates(xy.15N) <- ~ Longitude + Latitude
coordinates(xy.N) <- ~ Longitude + Latitude



# N15.LER <- raster::extract(LER, xy.15N)
pctN.LER <- raster::extract(LER, xy.N)
pctN.forest <- raster::extract(forest, xy.N)


# N15points$LER <- N15.LER
pctNpoints$LER <- pctN.LER
pctNpoints$forest <- factor(pctN.forest)


# points <- rbind(cbind(N15points,type = rep('N15')),
#                 cbind(pctNpoints,type = rep('pctN'))
# )
# points <- cbind(pctNpoints,type = rep('pctN'))
points <- pctNpoints




# make loop

for (i in seq(nrow(points))) {
     
     # level 0:     no montane
     if (points$MASL[i] < 300) {points$Montane[i] <- 'lowland'}
     
     # level 1:     elevation 300 - 999
     #              LER == 1
     else if (points$MASL[i] >= 300 & points$MASL[i] < 1000) {
          if (points$LER[i] == 1) {
               points$Montane[i] <- 'montane'
          } else {
               points$Montane[i] <- 'lowland'}
     }
     
     # level 2:     elevation 1000 - 1499
     #              LER == 1 OR slope > 5
     else if (points$MASL[i] >= 1000 & points$MASL[i] < 1500) {
          if (points$LER[i] == 1 | points$slope[i] > 5) {
               points$Montane[i] <- 'montane'
          } else {
               points$Montane[i] <- 'lowland'}
     }
     
     # level 3:     elevation 1500 - 2499
     #              slope > 2
     else if (points$MASL[i] >= 1500 & points$MASL[i] < 2500) {
          if(points$slope[i] > 2) {
               points$Montane[i] <- 'montane'
          } else {
               points$Montane[i] <- 'lowland'}
     }
     
     # level 4:     elevation > 2500
     else if (points$MASL[i] >= 2500) {points$Montane[i] <- 'montane'}
}


str(points)
# N15points <- points %>% filter(type == 'N15')
# pctNpoints <- points %>% filter(type == 'pctN')

original.data <- read_csv('15NDataSummarized_original.csv') %>%
        mutate(id = rep(1:length(Latitude))) %>%
        filter(id %in% points$bc_id)
points <- points %>% cbind.data.frame(original.data[,3:12])


points %>% write_csv('TransectN_points_classified_July2020.csv')

