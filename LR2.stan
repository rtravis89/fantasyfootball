data {
  int<lower = 1> N;
  int<lower = 1> N_pos;
  int<lower = 1, upper = 4> pos[N];
  int y[N];
}

//alpha = intercept.
parameters {
  vector[N_pos] alpha;
}

model {
  alpha ~ normal(0, 5);
  y ~ bernoulli_logit(alpha[pos]);
}

generated quantities {
  real p_hat_ppc = 0;
  real p_hat_QB_ppc = 0;
  real p_hat_RB_ppc = 0;
  real p_hat_TE_ppc = 0;
  real p_hat_WR_ppc = 0;
  
  {
    int n_QB = 0;
    int n_RB = 0;
    int n_TE = 0;
    int n_WR = 0;
    
    for (n in 1:N) {
      int y_ppc = bernoulli_logit_rng(alpha[pos[n]]);
      
      p_hat_ppc = p_hat_ppc + y_ppc;
      if (pos[n] == 1) {
        p_hat_QB_ppc = p_hat_QB_ppc + y_ppc;
        n_QB = n_QB + 1;
      }
      else if (pos[n] == 2) {
        p_hat_RB_ppc = p_hat_RB_ppc + y_ppc;
        n_RB = n_RB + 1;
      }
      else if (pos[n] == 3) {
        p_hat_TE_ppc = p_hat_TE_ppc + y_ppc;
        n_TE = n_TE + 1;
      }
      else {
        p_hat_WR_ppc = p_hat_WR_ppc + y_ppc;
        n_WR = n_WR + 1;
      }
      
    }
    p_hat_ppc = p_hat_ppc / (n_QB + n_RB + n_TE + n_WR);
    p_hat_QB_ppc = p_hat_QB_ppc / n_QB;
    p_hat_RB_ppc = p_hat_RB_ppc / n_RB;
    p_hat_TE_ppc = p_hat_TE_ppc / n_TE;
    p_hat_WR_ppc = p_hat_WR_ppc / n_WR;
  }
}

