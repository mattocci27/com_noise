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

# grouping by years and sites
dm.ys <- d %>%
  select(-1) %>%
  group_by(year, site) %>%
  summarise_each(funs(sum)) %>%
  as.data.frame
rownames(dm.ys) <- paste(dm.ys$year, dm.ys$site, sep = "_")
dm.ys <- dm.ys %>% select(-1:-2)

# species - site matrix
dm.year <- as.matrix(d.year[,-1])
rownames(dm.year) <- d.year$year
dm.year[,-1][dm.year[,-1] > 0] <- 1 # use 1 or 0

# species richness
sp.richness <- function(x){
  x[x > 0] <- 1
  apply(x, 1, sum)
}

# this substract species from the observed species
sp.trim <- function(dm, sp.vec, n.rep, sp.par){
  pool.richness <- as.integer(ncol(dm) * (1 - sp.par))
  sp.vec.trimmed <- sample(sp.vec, pool.richness)
  sp.vec.trimmed <- sp.vec.trimmed [order(names(sp.vec.trimmed))]
  temp.dat <- data.frame(sp = names(sp.vec.trimmed))
  temp.sp <- NULL
  for (i in 1:n.rep){
    # temp.sp <- sample(names(sp.vec.trimmed), n.sp, prob = sp.vec.trimmed, replace = replace) %>% unique
    for (j in 1:length(sp.vec.trimmed)){
      temp.vec <- c(sp.vec.trimmed[j], 1 - sp.vec.trimmed[j])
      names(temp.vec)[2] <- NA
      temp.sp[j] <- sample(names(temp.vec), 1, prob = temp.vec)
    }
    temp.sp2 <- na.omit(temp.sp)
    temp.dat2 <- data.frame(sp = temp.sp2, site = 1)
    names(temp.dat2)[2] <- paste("rep", i, sep = ".")
    suppressWarnings(temp.dat <- full_join(temp.dat, temp.dat2, by = "sp"))
  }
   #if all NA -> remove

  #  sp.not.apper <- apply(temp.dat[, -1], 1, sum, na.rm = T)
  #
   sp.name <- temp.dat %>% .$sp
   temp.dat[is.na(temp.dat) == T] <- 0
   sim.dat <- t(temp.dat[,-1])
  #  colnames(sim.dat) <- colnames(dm)
   colnames(sim.dat) <- sp.name
   return(sim.dat)
}

# --------------------------------------------------------------------------

# > dm.year %>% sp.richness
# 2011 2012 2013 2014
#   76   67   46   47

# example: remomving 20% of species
temp <- sp.trim(dm.year, sp.vec = sp.vec, n.rep = 10, sp.par = 0.2)



########
anosim_ES <- function(dm.year, dm.ys, sp.par = 0.2, n.rep = 10, n.runs = 999, ...){
  temp.gr <- c(rep("rand", n.rep), rep(2011:2014, each = 10)) %>%
    as.factor

  res <- numeric(n.runs)
  for (i in 1:n.runs){
    temp <- sp.trim(dm.year, sp.vec = sp.vec, n.rep = 10, sp.par = 0.1)
    temp2 <- merge(t(temp), t(dm.ys), by = "row.names") %>%
      t
    colnames(temp2) <- temp2[1, ]
    temp2 <- temp2[-1, ] %>%
      as.data.frame %>%
      apply(., 2, as.numeric)
    res[i] <- anosim(temp2, grouping = temp.gr, distance = "bray", permutations = 99)$statistic %>%
    as.numeric
  }
  list(ES1 = mean(res), ES2 = mean(res) / sd(res))
}

anosim_ES(dm.year, dm.ys, sp.par = 0.1, n.rep = 10, n.runs = 99)
