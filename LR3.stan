data {
  int<lower=1> N; //Number of rows
  int<lower=1> K; //Number of variables 
  matrix[N,K]  X; //Data matrix
  int<lower = 0, upper = 1> y[N]; // outcome
}

//alpha = intercept.
parameters {
  vector[K] beta; // vector of coefficients
  real alpha; // intercept
}

model {
  beta ~ normal(0, 5);
  alpha ~ normal(0, 5);
  y ~ bernoulli_logit(X * beta + alpha); 
}

