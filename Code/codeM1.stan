// STAN MODEL

data {
  int nteams;
  int ngames;
  vector[nteams] prior_score;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
  real df;
}
transformed data {
  vector[ngames] dif;
  vector[ngames] sqrt_dif;
  dif =  score1 - score2;
  for (i in 1:ngames)
    sqrt_dif[i] = (step(dif[i]) - .5)*sqrt(fabs(dif[i]));
}
parameters {
  real b;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
  vector[nteams] a;
}
model {
  a ~ normal(b*prior_score, sigma_a);
  for (i in 1:ngames)
    sqrt_dif[i] ~ student_t(df, a[team1[i]]-a[team2[i]], sigma_y);
}

generated quantities {
real team1rank;
real team2rank;
real rankdif;
vector[64] log_lik;

team1rank = normal_rng(b*prior_score[13],sigma_a);
team2rank = normal_rng(b*prior_score[3],sigma_a);
rankdif = team1rank - team2rank;

for (i in 1:64)
log_lik[i] = normal_lpdf(a[i] | prior_score[i],sigma_a);
}
