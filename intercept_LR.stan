data {
  int<lower = 1> N;
  int y[N];
}

//Intercept is the only parameter
parameters {
  real alpha;
}

model {
  alpha ~ normal(0, 5);
  y ~ bernoulli_logit(alpha);
}

generated quantities {
  real p_hat_ppc = 0;
  
  for (n in 1:N) {
    int y_ppc = bernoulli_logit_rng( alpha);
    p_hat_ppc = p_hat_ppc + y_ppc;
  }
  p_hat_ppc = p_hat_ppc / N;
}
