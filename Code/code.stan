// STAN MODEL

data {
  int nteams;
  int ngames;
  vector[nteams] prior_score;
  vector team1[ngames];
  vector team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
  real df;
}
transformed data {
  vector[ngames] dif;
  vector[ngames] sqrt_dif;
  dif <-  score1 - score2;
  for (i in 1:ngames)
    sqrt_dif[i] <- (step(dif[i]) - .5)*sqrt(fabs(dif[i]));
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

