####____PACKAGES____####
library(tidyverse)
library(data.table)
####___TOY DATA____####
df <- data.frame(dates = seq.Date(as.Date("2021-01-01"),as.Date("2022-12-01"),by = "month"),
                 number = seq(10,30,length.out=24),
                 cat = sample(c("1. Soñar","2. Jürgen","3. Adición"),size = 24,replace = TRUE),
                 rand = rnorm(24,1000000,5000),
                 nn = sample(c("1,2","2,4","3,0","3,5"),size = 24, replace = TRUE))

fwrite(df,"C:/Files/Data/test2.txt",quote = FALSE, sep = "|")
