# Script for Justin's tropical mountain isotope paper. 
# Analyses are based on Kapos et al., 2000.

source('~/R/startup.R') 
#d:/projects/tropical N cycling gay 2021/data/Raster

global <- raster('globalDEM23_5.tif')
slope <- raster('globalSlope23_5.tif')
degree2 <- slope >= 2
degree5 <- slope >= 5
LER.5k <- raster('globalLER23_5_5km.tif')
LER.10k <- raster('globalLER23_5_10km.tif')
k <- 0.9119327*0.9119327


# tropical area -----------------------------------------------------------

global.proj <- raster('globalDEM23_5_proj.tif') 
global.df <- global.proj %>% as.data.frame %>% filter(!is.na(globalDEM23_5_proj))

k*nrow(global.df) # 49,767,832 sq km of land in the tropics


# Zonation of tropical mountains ------------------------------------------

# Mountain elevation zones are defined by Kapos et al., 2000
# and based off of strictly elevation (above 2500) or both
# elevation and slope (below 2500). For mountains below 1500,
# a relief filter is applied (called a local elevation range).
# A cell below 1500 is deemed a mountain if LER greater than
# 300. 


# Level 6 is anything greater than 4500m
L6 <- global >= 4500 
writeRaster(L6, 'Elevation_Levels/L6.tif', overwrite = T)


# Level 5 is anything less than 4500m but greater than 3500m
L5 <- global < 4500 & global >= 3500
writeRaster(L5, 'Elevation_Levels/L5.tif', overwrite = T)


# Level 4 is anything less than 3500m but greater than 2500m
L4 <- global < 3500 & global >= 2500
writeRaster(L4, 'Elevation_Levels/L4.tif', overwrite = T)


# Level 3 is anything less than 2500m but greater than 1500m
# with a slope greater than 2 degrees
S3 <- global < 2500 & global >= 1500
L3 <- S3 * degree2
writeRaster(L3, 'Elevation_Levels/L3.tif', overwrite = T)



# Using a LER of 5km ------------------------------------------------------

# Level 2 is anything less than 1500m but greater than 1000m
# with a slope greater than 5 degrees or a LER of 300
S2 <- global < 1500 & global >= 1000
L2a <- S2 * degree5
L2b <- S2 * LER.5k
L2 <- L2a + L2b
L2[L2>0] <- 1
writeRaster(L2, 'Elevation_Levels/L2_5k.tif', overwrite = T)



# Level 1 is anything less than 1000 but greater than 300m 
# with a LER of 300
S1 <- global < 1000 & global >= 300
L1 <- S1 * LER.5k
writeRaster(L1, 'Elevation_Levels/L1_5k.tif', overwrite = T)


# Using a LER of 10km ------------------------------------------------------

# Level 2 is anything less than 1500m but greater than 1000m
# with a slope greater than 5 degrees or a LER of 300
S2 <- global < 1500 & global >= 1000
L2a <- S2 * degree5
L2b <- S2 * LER.10k
L2 <- L2a + L2b
L2[L2>0] <- 1
writeRaster(L2, 'Elevation_Levels/L2_10k.tif', overwrite = T)



# Level 1 is anything less than 1000 but greater than 300m 
# with a LER of 300
S1 <- global < 1000 & global >= 300
L1 <- S1 * LER.10k
writeRaster(L1, 'Elevation_Levels/L1_10k.tif', overwrite = T)




# Cell analysis -----------------------------------------------------------
# L1 <- raster('Elevation_Levels/L1_10k.tif') 
L1 <- raster('Elevation_Levels/L1_5k.tif')
# L2 <- raster('Elevation_Levels/L2_10k.tif') 
L2 <- raster('Elevation_Levels/L2_5k.tif')
L3 <- raster('Elevation_Levels/L3.tif')
L4 <- raster('Elevation_Levels/L4.tif')
L5 <- raster('Elevation_Levels/L5.tif')
L6 <- raster('Elevation_Levels/L6.tif')

L6 %>% as.data.frame() %>% filter(L6 == 1) %>% dim # 137,249
k * 137249 # 114,139
L5 %>% as.data.frame() %>% filter(L5 == 1) %>% dim # 559,921
k * 559921 # 465,642
L4 %>% as.data.frame() %>% filter(L4 == 1) %>% dim # 517,098
k * 517098 # 430,030
L3 %>% as.data.frame() %>% filter(L3 == 1) %>% dim # 1,429,402
k * 1429402 # 1,188,721
# 5km
L2 %>% as.data.frame %>% filter(L2_5k == 1) %>% dim # 1,152,823
k * 1152823 # 958,712
L1 %>% as.data.frame %>% filter(L1_5k == 1) %>% dim # 1,321,090
k * 1321090 # 1,098,647

# 10km
# L2 %>% as.data.frame() %>% filter(L2_10k == 1) %>% dim # 1,529,583 
# L1 %>% as.data.frame() %>% filter(L1_10k == 1) %>% dim # 2,262,492

k * (137249+559921+517098+1429402+1152823+1321090)

(137249+559921+517098+1429402+1152823+1321090)/59991855 # percent mountains: 8.53 % with small LER

# (137249+559921+517098+1429402+1529583+2262492)/59991855 # percent mountains: 10.73 % with larger LER





# Forest Analysis ---------------------------------------------------------

# assuming 10k
# 
# L1 <- raster('Elevation_Levels/L1_10k.tif')
# L2 <- raster('Elevation_Levels/L2_10k.tif')
# L3 <- raster('Elevation_Levels/L3.tif')
# L4 <- raster('Elevation_Levels/L4.tif')
# L5 <- raster('Elevation_Levels/L5.tif')
# L6 <- raster('Elevation_Levels/L6.tif')
# 
# dem <- raster('globalDEM23_5.tif')
# 
# 
# 

# 
# 
# mountains <- L1 + L2 + L3 + L4 + L5 + L6
# writeRaster(mountains, 'Global_mountains.tif', overwrite = T)



mtns <- raster('Global_mountains.tif') 
tc <- raster('TC/trop_tc.tif')
# tc.30 <- tc>=10
# writeRaster(tc.30, 'TC/trop_tc_30.tif')
map <- raster('trop_map.tif')
map1500 <- map>=1500
# mtns.tc <- mtns * tc
# writeRaster(mtns.tc, 'TC/trop_tc_mtn.tif', overwrite = T)
mtns.tc <- raster('TC/trop_tc_mtn.tif')

tc.10 <- raster('TC/trop_tc_10.tif')
tc.10.map <- tc.10*map1500
writeRaster(tc.10.map, 'TC/trop_tc_map1500.tif')

# mtns.tc10 <- tc.10*mtns
# writeRaster(mtns.tc10, 'TC/trop_mtns_tc_10.tif', overwrite = T)
mtns.tc10 <- raster('TC/trop_mtns_tc_10.tif')
mtns.tc10.map <- mtns.tc10*map1500


mtns %>% as.data.frame %>% filter(Global_mountains == 1) %>% dim # 5,115,722

tc.10 %>% as.data.frame %>% filter(trop_tc_10 == 1) %>% dim # 31,864,239

tc.30 %>% as.data.frame %>% filter(layer == 1) %>% dim # 23,215,116

tc.10.map %>% as.data.frame %>% filter(layer == 1) %>% dim # 18,739,961

mtns.tc10 %>% as.data.frame %>% filter(layer == 1) %>% dim # 3,316,234

mtns.tc10.map %>% as.data.frame %>% filter(layer == 1) %>% dim # 1,795,571








# tropical tree cover (in pixels) : 42,900,796
# tropical tree cover > 10%       : 31,864,239 


# mountainous cover               : 5,115,772 (8.53% of tropics)
# mountainous tree cover          : 4,035,872 (78.9% of mountainous tropics)
# mountainous tree cover > 10%    : 3,316,234 (67.2% of mountainous tropics is forested)




