#DATAAAAAA
library(rstan)
library(arm)
library(matrixStats)

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
nationality = as.integer(W2019teamrank$Nationality)
df = 7

data <- list(nteams=nteams, ngames = ngames , team1 = team1 , score1 = score1 , team2=team2 , score2=score2 , prior_score=prior_score,df=df, nationality=nationality)

###########
# Model 1 #
###########

fit_bern1 <- stan(file='codeM1.stan', data=data)
print(fit_bern1)

# Reduced version for the Rhat printing
print(fit_bern1,pars=c("b","sigma_a","sigma_y","a[1]","a[2]",
                       "a[3]","a[62]","a[63]","a[64]"))


# Finals prediction code #
# Model 1 #
pred_fit <- extract(fit_bern1)
rankdif_fit <- get_posterior_mean(fit_bern1,par=c("rankdif"))[1]
sigma_y_fit <- get_posterior_mean(fit_bern1,par=c("sigma_y"))[1]
# a[13] - a[3] #
pred_draw <- rt(10000,7)

pred_rankdif <- rankdif_fit + sqrt(sigma_y_fit)*pred_draw
sqrt_dif_find_ptpred <- ((abs(mean(pred_rankdif)) * 2)^2)* sign(mean(pred_rankdif))
sqrt_dif_find <- ((abs(pred_rankdif) * 2)^2)* sign(pred_rankdif)
hist(pred_rankdif,xlim=c(-3,3),breaks=100)
print(fit_bern1,pars=c("rankdif"))

# Log-likelihood
ll_fit <- extract(fit_bern1)
fit_extract <- extract(fit_bern1)
ll_fit_loo <- loo(ll_fit$log_lik)

teams <- W2019teamrank$Team
teamsRank <- W2019teamrank$TeamRank

# Looking at the team quality estimate
a_sims <- fit_extract$a
a_hat <- colMeans(a_sims)
a_se <- sqrt(colVars(a_sims))
# Appending the variables to a 
teams.append <- data.frame(a_hat=a_hat,a_se=a_se,teams,teamsRank) 
teams.sort <- teams.append[order(a_hat),]
library ("arm")
# Edit plot so that it is ordered by team rank #
coefplot (teams.sort$a_hat, teams.sort$a_se, CI=1, varnames=teams.sort$teamsRank, main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2))

# Looking at the 
a_sims <- fit_extract$a
sigma_y_sims <- fit_extract$sigma_y
nsims <- length(sigma_y_sims)
random_outcome <- array(NA, c(nsims,ngames))
for (s in 1:nsims){
  random_outcome[s,] <- (a_sims[s,team1] - a_sims[s,team2]) + rt(ngames,df)*sigma_y_sims[s]
}
sim_quantiles <- array(NA,c(ngames,2))
for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}

new_order <- sample(teams,64)

png ("Wimbledon.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order]*flip, sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1]*flip, upper.conf.bounds=sim_quantiles[new_order,2]*flip, 
          varnames=ifelse(flip==1, paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
                          paste(teams[team2[new_order]], "vs.", teams[team1[new_order]])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2), xlim=c(-6,6))

###########
# Model 2 #
###########

fit_bern2 <- stan(file='codeM2.stan', data=data)
print(fit_bern2)


pred_fit <- extract(fit_bern2)
pred_draw <- rt(10000,7)
pred_rankdif <- -0.097 + sqrt(0.45)*pred_draw
sqrt_dif_find_ptpred <- ((abs(mean(pred_rankdif)) * 2)^2)* sign(mean(pred_rankdif))
sqrt_dif_find <- ((abs(pred_rankdif) * 2)^2)* sign(pred_rankdif)
hist(pred_rankdif,xlim=c(-2,2),breaks=100)


