data {
  int<lower=1> ng;
  int<lower=1> nt;
  array[ng, 2] int x; // opponent numbers
  array[ng, 2] int y; // scores
}

parameters {
  array[nt] real o;
  array[nt] real d;
  real h;
}

model {
  int t1;
  int t2;
  real eta1;
  real eta2;
  real nu1;
  real nu2;
  // priors
  o ~ normal(0, 1);
  d ~ normal(0, 1);
  h ~ normal(0, 1); // a priori more equal
  // likelihood
  for (i in 1:ng)
  {
    t1 = x[i,1];
    t2 = x[i,2];
    nu1 = h + o[t1] - d[t2];
    nu2 = o[t2] - d[t1];
    eta1 = exp(nu1);
    eta2 = exp(nu2);
    y[i,1] ~ poisson(eta1);
    y[i,2] ~ poisson(eta2);
  }
}
