#DATAAAAAA

# Players with rank
W2019players <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/Wimbledon%202019%20Doubles%20Players%20240619.csv')

# Tournament results
W2019results <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/Wimbledon%202019%20Match%20Results.csv')

# Team rank
W2019teamrank <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/TeamRank.csv')

# Player - team
W2019playerteam <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/PlayerTeam.csv')

# Wimbledon games
W2019teamresults <- read.csv('https://raw.githubusercontent.com/mcnaughtonshaun/BDA-Assignment/master/Data/TeamResult.csv')
# Dropping the final match as we are trying to predict the winner
W2019teamresults <- W2019teamresults[1:62,]


# Ranking transformation
ranks <- (W2019teamrank$TeamRating  - mean(W2019teamrank$TeamRating ))/(2*sd(W2019teamrank$TeamRating ))

nteams = length(W2019teamrank$Team)
ngames = 62
prior_score = ranks
team1 = W2019teamresults$Team1
team2 = W2019teamresults$Team2
score1 = W2019teamresults$SetT1
score2 = W2019teamresults$SetT2
df = 7

data <- list(nteams=nteams, ngames = ngames , team1 = team1 , score1 = score1 , team2=team2 , score2=score2 , prior_score=prior_score,df=df)

fit_bern <- stan(file='codeM1.stan', data=data)
print(fit_bern)

# Finals prediction code #
pred_fit <- extract(fit_bern)
pred_draw <- rt(10000,7)
pred_rankdif <- -0.097 + sqrt(0.45)*pred_draw
sqrt_dif_find_ptpred <- ((abs(mean(pred_rankdif)) * 2)^2)* sign(mean(pred_rankdif))
sqrt_dif_find <- ((abs(pred_rankdif) * 2)^2)* sign(pred_rankdif)
hist(pred_rankdif,xlim=c(-2,2),breaks=100)

ll_fit <- extract(fit_bern)
ll_fit_loo <- loo(ll_fit$log_lik)

                       
