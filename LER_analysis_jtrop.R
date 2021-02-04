# LER analysis ------------------------------------------------------------

global.mat <- read.csv('c:/Users/bryce/Dropbox/Justin_Isotope/Data/GlobalDEM23_5_mat.csv')
gm <- global.mat

# global.mat2 <- global.mat[2000:2300, 25000:25300]
# gm <- global.mat2

# LER is a 10 sq km window that checks for a relief of 300m
# around each cell. If 300m of relief is found within the LER window, the cell
# is deemed a mountain.

for (i in seq(nrow(gm))) {
  for (j in seq(ncol(gm))) {
    
    if (is.na(gm[i,j])) {global.mat2[i,j] <- NA
    
    } else {
      
      # this section of logical statements checks for the edges
      # of the map and adjusts the LER window
      if (i < 5) {i.seq <- i:(i+5)
      } else if (i > nrow(gm)-5) {i.seq <- (i-5):nrow(gm)
      } else {i.seq <- (i-5):(i+5)
      }
      
      
      if (j < 5) {j.seq <- j:(j+5)
      } else if (j > ncol(gm)-5) {(j-5):ncol(gm)
      } else {j.seq <- (j-5):(j+5)
      }
      
      LER <- gm[i.seq, j.seq]
      
      if (sum(is.na(LER)) == (nrow(LER)*ncol(LER))) {global.mat[i,j] <- NA} else {relief <- gm[i,j] - min(LER, na.rm = T)}
      
      
      if (relief >= 300) {global.mat[i,j] <- 1} else {global.mat[i,j] <- NA}
    }
  }
}

write.csv(global.mat, 'globalLER23_5.csv')