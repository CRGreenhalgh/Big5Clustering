#############################
### Presentation Graphics ###
#############################

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)

### Reading in Data
sumNorm <- read_csv("normalized.csv") %>% select(-X1)
gathered <- sumNorm %>% gather(Construct, Values, EXT:OPN)
summed <- read_csv("summed.csv")
gathered2 <- summed %>% gather(Construct, Values, EXT:OPN)

### Histograms
gathered %>% ggplot(mapping = aes(x = Values, fill = Construct)) + geom_histogram(color = "black") + 
  facet_wrap(vars(Construct), scales = "free")

gathered2 %>% ggplot(mapping = aes(x = Values, fill = Construct)) + geom_histogram(color = "black") + 
  facet_wrap(vars(Construct), scales = "free")
