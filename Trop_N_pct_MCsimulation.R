
# Option 1. Geometric mean and sd from distributions -----------------------------------

rm(list = ls())
cat('\014')


library(dplyr)
library(readr)
library(fitdistrplus)
library(ggplot2)
source('~/R/gm_mean.R')

alltrop <- read_csv('c:/users/bryce/onedrive/documents/other projects/Tropical N cycling Gay 2021/alltrop.2.csv')


# Montane
mtn.dist <- alltrop %>% 
        filter(mountains == 'montane') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
     unlist %>% na.exclude() %>% as.numeric %>%
     fitdist(distr = 'lnorm')
mtn.dist$estimate
# meanlog     sdlog 
# 28.6426834  0.7616803

# 2.63 Tg N (range: 0.0029 Tg - 1997 Tg N) or (2.9 Gg - 2.0 Pg)

ggplot(alltrop %>% filter(mountains == 'montane')) +
     geom_histogram(aes(x = total_N_pool, ..density..)) +
     stat_function(fun = dlnorm, args = list(meanlog = mtn.dist$estimate[1], sdlog = mtn.dist$estimate[2]), 
                   color = "red", size = 1) +
     theme_bw() +
     labs(x = 'Montane N pool')


# lowland
lld.dist <- alltrop %>%
        filter(mountains == 'lowland') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
     unlist %>% na.exclude() %>% as.numeric %>%
     fitdist(distr = 'lnorm')
lld.dist$estimate
# meanlog      sdlog 
# 29.9803940  0.7630515 

# 10.5 Tg N (range: 0.0086 Tg N - 11589 Tg N) or (8.6 Gg - 11.6 Pg)

ggplot(alltrop %>% filter(mountains == 'lowland')) +
     geom_histogram(aes(x = total_N_pool, ..density..)) +
     stat_function(fun = dlnorm, args = list(meanlog = lld.dist$estimate[1], sdlog = lld.dist$estimate[2]), 
                   color = "red", size = 1) +
     theme_bw() +
     labs(x = 'Lowland N pool')



# MC x times
times <- 10000
tropNpct <- numeric(length = times)
mtn.val <- numeric(length = times)
lld.val <- numeric(length = times)

for (i in seq(times)) {
     
     # simulate montane and lowland vals from distribution
     mtn.val[i] <- rlnorm(1, meanlog = mtn.dist$estimate[1], sd = mtn.dist$estimate[2])
     lld.val[i] <- rlnorm(1, meanlog = lld.dist$estimate[1], sd = lld.dist$estimate[2])
     
     # percent tropical N
     tropNpct[i] <- mtn.val[i]/(mtn.val[i]+lld.val[i])*100
}

hist(tropNpct)
gm.mean(tropNpct)
gm.sd(tropNpct)


# 19.1% (range: 8.49 - 42.98%)




# Option 2. Using CLT and Sampling from distributions -----------------------------------


rm(list = ls())
cat('\014')


library(dplyr)
library(readr)
library(fitdistrplus)
library(ggplot2)
source('~/R/gm_mean.R')

alltrop <- read_csv('c:/users/bryce/onedrive/documents/other projects/Tropical N cycling Gay 2021/alltrop.2.csv')


# Montane
mtn.dist <- alltrop %>% 
        filter(mountains == 'montane') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
        unlist %>% na.exclude() %>% as.numeric %>%
        fitdist(distr = 'lnorm')
mtn.dist$estimate
# meanlog     sdlog 
# 28.6426834  0.7616803

ggplot(alltrop %>% filter(mountains == 'montane')) +
        geom_histogram(aes(x = total_N_pool, ..density..)) +
        stat_function(fun = dlnorm, args = list(meanlog = mtn.dist$estimate[1], sdlog = mtn.dist$estimate[2]), 
                      color = "red", size = 1) +
        theme_bw() +
        labs(x = 'Montane N pool')


# lowland
lld.dist <- alltrop %>%
        filter(mountains == 'lowland') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
        unlist %>% na.exclude() %>% as.numeric %>%
        fitdist(distr = 'lnorm')
lld.dist$estimate
# meanlog      sdlog 
# 29.9803940  0.7630515 

ggplot(alltrop %>% filter(mountains == 'lowland')) +
        geom_histogram(aes(x = total_N_pool, ..density..)) +
        stat_function(fun = dlnorm, args = list(meanlog = lld.dist$estimate[1], sdlog = lld.dist$estimate[2]), 
                      color = "red", size = 1) +
        theme_bw() +
        labs(x = 'Lowland N pool')



# MC 10,000 times
times <- 10000

mtn.val <- rlnorm(times, meanlog = mtn.dist$estimate[1], sd = mtn.dist$estimate[2])
lld.val <- rlnorm(times, meanlog = lld.dist$estimate[1], sd = lld.dist$estimate[2])

# percent tropical N
mtnNpct <- mtn.val/(mtn.val+lld.val)*100

hist(mtnNpct); summary(mtnNpct)


# Using CLT to calculation mean+/-SD of N pools 


mtnNpct.clt <- numeric(length = times)
mtn.clt <- numeric(length = times)
lld.clt <- numeric(length = times)

sample.size <- 30 # increasing sample size decreases uncertainty - how to select this number?

for (i in seq(times)){
        mtnNpct.clt[i] <- mean(sample(mtnNpct, size = sample.size)) 
        mtn.clt[i] <- mean(sample(mtn.val, size = sample.size))
        lld.clt[i] <- mean(sample(lld.val, size = sample.size))
}


hist(mtn.clt)
hist(lld.clt)
hist(mtnNpct.clt)

mean(mtnNpct.clt); sd(mtnNpct.clt) # 25.2 +/- 3.2%
mean(mtn.clt); sd(mtn.clt) # 3.69 +/- 0.6 Tg N
mean(lld.clt); sd(lld.clt) # 14.02 +/- 2.3 Tg N



# Option 3. Using CLT and sampling from only data -----------------------


rm(list = ls())
cat('\014')


library(dplyr)
library(readr)
library(fitdistrplus)
library(ggplot2)
source('~/R/gm_mean.R')

alltrop <- read_csv('c:/users/bryce/onedrive/documents/other projects/Tropical N cycling Gay 2021/alltrop.2.csv')




mtn.val <- alltrop %>% 
        filter(mountains == 'montane') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
        unlist %>% na.exclude() %>% as.numeric

lld.val <- alltrop %>%
        filter(mountains == 'lowland') %>%
        filter(MAP >= 1200) %>%
        filter(forest == 1) %>%
        dplyr::select(total_N_pool) %>%
        unlist %>% na.exclude() %>% as.numeric


total.val <- sample(mtn.val, 144, replace = T) + sample(lld.val, 144, replace = T)


mtnNpct <- mtn.val/total.val*100; hist(mtnNpct)


times <- 10000
mtnNpct.clt <- numeric(length = times)
mtn.clt <- numeric(length = times)
lld.clt <- numeric(length = times)

sample.size <- 20 # increasing sample size decreases uncertainty - how to select this number?

for (i in seq(times)){
        mtnNpct.clt[i] <- mean(sample(mtnNpct, size = sample.size)) 
        mtn.clt[i] <- mean(sample(mtn.val, size = sample.size))
        lld.clt[i] <- mean(sample(lld.val, size = sample.size))
}



hist(mtn.clt)
hist(lld.clt)
hist(mtnNpct.clt)

mean(mtnNpct.clt); sd(mtnNpct.clt)
mean(mtn.clt); sd(mtn.clt)
mean(lld.clt); sd(lld.clt)


