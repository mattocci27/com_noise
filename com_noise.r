rm(list = ls()) # This clears everything from memory.
# install.packages("dplyr")
library(dplyr)
library(vegan)
setwd("~/Dropbox/com_noise/")

d <- read.csv("Khao Chong Formi 12March2015.csv")
d[,-1][d[,-1] > 0] <- 1 # use 1 or 0

# A-E upper stream, F-K lower stream
d$site <- sapply(strsplit(as.character(d$X)," "), "[", 1) #new col for site
d$year <- sapply(strsplit(as.character(d$X)," "), "[", 2) #new col for year

# species occurence vec
sp.vec <- apply(d[,2:(ncol(d)-2)], 2, sum) / 40

# grouping by years
d.year <- d %>%
  select(-1) %>%
  select(-site) %>%
  group_by(year) %>%
  summarise_each(funs(sum))

# species - site matrix
dm.year <- as.matrix(d.year[,-1])
rownames(dm.year) <- d.year$year
dm.year[,-1][dm.year[,-1] > 0] <- 1 # use 1 or 0


# functions -----------------------------------------------------------------
# species richness
sp.richness <- function(x){
  x[x > 0] <- 1
  apply(x, 1, sum)
}

sp.par <- 0.1

dm <- dm.year

# this substract species from the observed species
sp.trim <- function(dm, n.sp, n.rep, sp.par){
  pool.richness <- as.integer(ncol(dm) * (1 - sp.par))
  temp.dat <- data.frame(sp = names(sp.vec))
  for (i in 1:n.rep){
    temp.sp <- sample(colnames(dm), n.sp, prob = sp.vec)
    temp.dat2 <- data.frame(sp = temp.sp, site = 1)
    names(temp.dat2)[2] <- paste("site", i, sep = ".")
    suppressWarnings(temp.dat <- full_join(temp.dat, temp.dat2, by = "sp"))
  }
   temp.dat[is.na(temp.dat) == T] <- 0
   sim.dat <- t(temp.dat[,-1])
   colnames(sim.dat) <- colnames(dm)
  #  rownames(sim.dat) <- rownames(dm)
   return(sim.dat)
}


# --------------------------------------------------------------------------

# > dm.year %>% sp.richness
# 2011 2012 2013 2014
#   76   67   46   47

# example: remomving 20% of species
sp.trim(dm.year, n.sp = 76, n.rep = 3, sp.par = 0.2) %>% sp.richness

sp.trim(dm.year, n.sp = 76, n.rep = 3, sp.par = 0.2)
