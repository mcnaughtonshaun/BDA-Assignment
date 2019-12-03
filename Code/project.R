install.packages("readr")
install.packages("httr")


#read_csv requires readr

# Players with rank
W2019players <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/Wimbledon%202019%20Doubles%20Players%20240619.csv')

# Tournament results
W2019results<- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/Wimbledon%202019%20Match%20Results.csv')

