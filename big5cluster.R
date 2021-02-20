#############################
#### Clean Big Five Data ####
#############################

#### Libraries I Need ####
library(tidyverse)
library(DataExplorer)

#### Read and Clean Data ####
dat <- read_tsv("data-final.csv")
ques <- read.delim("codebook.txt")

#### Filter out IPC and remove non-question variables
dat <- dat %>% filter(IPC == 1) %>% 
  select(1:50)

#### Look at missing data. All missing values are given a 0
plot_missing(dat)
table(dat$EXT1)

#### Turn all 0s into NAs
dat[dat == 0] <- NA

#### Count number of NAs in a row and remove any row with at least one NA
dat$na_count <- apply(dat, 1, function(x) sum(is.na(x)))
dat <- dat %>% filter(na_count == 0)

#### Drop NA_Count variable
dat <- dat %>% select(-na_count)

####
neg_questions <- c(2,4,6,8,10,12,14,21,23,25,27,32,34,36,38,42,44,46)
dat[,c(2,4,6,8,10,12,14,21,23,25,27,32,34,36,38,42,44,46)] <- (dat[,c(2,4,6,8,10,12,14,21,23,25,27,32,34,36,38,42,44,46)] * -1) + 6
dat <- dat - 3

#### Create sum variables
dat_new <- dat %>%
  mutate(EXT_SUM = select(., EXT1:EXT10) %>% 
           rowSums(), 
         EST_SUM = select(., EST1:EST10) %>% 
           rowSums(),
         AGR_SUM = select(., AGR1:AGR10) %>% 
           rowSums(),
         CSN_SUM = select(., CSN1:CSN10) %>% 
           rowSums(),
         OPN_SUM = select(., OPN1:OPN10) %>% 
           rowSums()) %>% 
  select(EXT_SUM, EST_SUM, AGR_SUM, CSN_SUM, OPN_SUM)

#### Write out clean data
write.csv(dat, "./big5clean_all.csv")
write.csv(dat_new, "./big5clean_construct.csv")
