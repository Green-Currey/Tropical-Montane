
# Read in DEM
mtn <- raster('c:/users/bryce/OneDrive/Desktop/J/Global_mountains.tif')
dem <- raster('c:/users/bryce/Dropbox/GIS_data/Misc/Global_DEM/Global_DEM.tif')
     
     
# Create Aspect raster layer
slp <- terrain(dem, opt = 'slope', unit = 'degrees')

# Read in data with coordinates
data <- read_csv('c:/users/bryce/onedrive/desktop/j/peru.csv')

# Extract coords
coords <- data %>% dplyr::select(long, lat)

# Turn coordinates into spatial points using coordinates function
coordinates(coords) <- ~ long + lat

# extract aspect values from aspect raster
slp.vals <- raster::extract(slp, coords)
mtn.vals <- raster::extract(mtn, coords)

# Add aspect values to data frame
data$Slope <- slp.vals
data$mountains <- mtn.vals

# ammendments
data$mountains <- ifelse(data$mountains==1, 'montane', 'lowland')
# data[9,'Slope'] <- 0
# data[9,'mountains'] <- 'lowland'

data

# update spreadsheet
write_csv(data, 'c:/users/bryce/OneDrive/Desktop/J/peru_bc.csv')
