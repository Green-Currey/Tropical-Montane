trop.all <- read_csv('c:/users/bryce/OneDrive/Desktop/j/transectN_05272020.csv')
trop <- read_csv('c:/users/bryce/OneDrive/Desktop/j/TropN_points.csv')
dem.gee <- read_csv('c:/users/bryce/OneDrive/Desktop/j/TropN_points_DEM.csv')
slope.gee <- read_csv('c:/users/bryce/OneDrive/Desktop/j/TropN_points_slope.csv')


dem.gee$num <- str_remove(string = dem.gee$id, pattern = "Point_") %>% as.numeric
dem.gee <- dem.gee %>% arrange(num)

slope.gee$num <- str_remove(string = slope.gee$id, pattern = "Point_") %>% as.numeric
slope.gee <- slope.gee %>% arrange(num)
