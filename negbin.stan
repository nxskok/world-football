

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  array[N] int y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> mu;
  real<lower=0> phi;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu ~ gamma(1, 2);
  phi ~ gamma(1, 1);
  y ~ neg_binomial_2(mu, phi);
}

generated quantities {
  int<lower=0> y_draw = neg_binomial_2_rng(mu, phi);
}
