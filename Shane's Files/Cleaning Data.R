# Big Five Data Cleaning

library(tidyverse)

data_raw <- read_tsv('IPIP-FFM-data-8Nov2018/data-final.csv')

data <- data_raw

# Number of rows with NA
#sum(!complete.cases(data))

# Drop NAs and 0s
data[data==0] <- NA
data <- data %>% drop_na()

# Only keep IPC==1
data <- data[data$IPC==1,]

# Only keep IPIP questions
data <- data[,0:50]

## Adjusting scoring system
pos_questions = c(
    'EXT1','EXT3','EXT5','EXT7','EXT9',                       # 5
    'EST1','EST3','EST5','EST6','EST7','EST8','EST9','EST10', # 8
    'AGR2','AGR4','AGR6','AGR8','AGR9','AGR10',               # 6
    'CSN1','CSN3','CSN5','CSN7','CSN9','CSN10',               # 6
    'OPN1','OPN3','OPN5','OPN7','OPN8','OPN9','OPN10'         # 7
)

neg_questions = c( 
    'EXT2','EXT4','EXT6','EXT8','EXT10', # 5
    'EST2','EST4',                       # 2
    'AGR1','AGR3','AGR5','AGR7',         # 4
    'CSN2','CSN4','CSN6','CSN8',         # 4
    'OPN2','OPN4','OPN6'                 # 3
)


data <- data %>% mutate_at(pos_questions ,~recode(., `1`=-2, `2`=-1, `3`=0, `4`=1, `5`=2))
data <- data %>% mutate_at(neg_questions ,~recode(., `1`=2, `2`=1, `3`=0, `4`=-1, `5`=-2))

write_csv(data, 'IPIP-FFM-data-8Nov2018/data-clean.csv')


## Average results for each category
ext_mean <- rowMeans(data %>% select(1:10))
est_mean <- rowMeans(data %>% select(11:20))
arg_mean <- rowMeans(data %>% select(21:30))
csn_mean <- rowMeans(data %>% select(31:40))
opn_mean <- rowMeans(data %>% select(41:50))

data_mean <- data.frame("EXT"=ext_mean, "EST"=est_mean, "AGR"=arg_mean, "CSN"=csn_mean, "OPN"=opn_mean)

rm(ext_mean, est_mean, arg_mean, csn_mean, opn_mean)

write_csv(data_mean, 'IPIP-FFM-data-8Nov2018/data-clean-means.csv')






