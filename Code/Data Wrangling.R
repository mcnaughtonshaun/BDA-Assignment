install.packages("readr")
install.packages("httr")


#read_csv requires readr
W2019results <-read.csv('C:/Git/BDA-project/Data/Wimbledon 2019 Match Results.csv')

W2019results <- read.csv(url='https://raw.github.com/mcnaughtonshaun/BDA-Assignment/blob/master/Data/Wimbledon%202019%20Match%20Results.csv')

library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
y <- read.csv(text = x)