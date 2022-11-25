data {
    int<lower=0> J; // Coders
    int<lower=0> N; // N
    int<lower=0> k; // min grade
    int<lower=k> K; // max grade
    int<lower=-1,upper=K> wdata[N,J]; // data
}

  parameters {
    vector[N] Z;
    real<lower=0> tau[J]; //coder sd
    real<lower=0> beta[J]; //coder slope
    vector[J] alpha; //coder intercept
    real<lower=0> a; //sd shape hyperparameter
    real<lower=0> b; //sd rate hyperparameter
  }

  model {

    a ~ gamma(.1,.1); // sd shape hyperparameter
    b ~ gamma(.1,.1);// sd rate hyperparameter
    for(i in 1:N) {
      Z[i] ~ normal(0, 1);
    }

for (j in 1:J) {
      beta[j] ~ normal(0,30); // coder slope
      alpha[j] ~ normal(0,30); //coder intercept
      tau[j] ~ gamma(a,b); //coder sd

      for (i in 1:N) if (wdata[i,j] != -1) {
        wdata[i,j] ~ normal(alpha[j] + beta[j]*Z[i], tau[j]); //inverse-gamma sd
      }
    }
}
