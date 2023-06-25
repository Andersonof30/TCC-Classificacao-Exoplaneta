// This Stan program defines a regression ASN model, with a

functions {
  real DSjP_log(vector x, vector p) {
    vector[num_elements(x)] prob;
    real lprob;
    for (i in 1:num_elements(x)) {
      prob[i] = p[i]^x[i]*(1-p[i])^(1-x[i]);
    }
    lprob = sum(log(prob));
    return lprob;
  }
  int signnum(real x) { return x < 0 ? -1 : x > 0; } 
}

// The input data is a matrix 'X' of size 'N x 6' and a vector 'Y' of length 'N'.
data {
  int<lower=0> N;
  matrix[N, 6] X;
  vector[N] Y;
  real <lower=0> tau; // hiperparâmetro Lasso
}

// The parameters accepted by the model. Our model
// accepts five parameters 'beta0', 'beta1', 'beta2', 
// 'beta3', 'sigma' and 'alpha'.
parameters {
  vector[7] beta; // beta0 to beta6
  real lambda;
}

// The adopted transformation in the location parameter 'mu'
transformed parameters {
  vector[N] p;
  for (i in 1:N) {
    real x = beta[1] + dot_product(X[i], beta[2:]);
    p[i] = (((1/16)*(exp(-x)*(-x^2 - 3*x + 8*exp(x) - 4)*(sign(x) + 1) - exp(x)*(x^2 - 3*x + 4)*(sign(x) - 1))))^exp(lambda);

  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu',
// standard deviation 'sigma' and asymmetry 'alpha'.
model {
  //PRIORI
  for (j in 1:7) {
    beta[j] ~ double_exponential(0, tau);
  }
  lambda ~ uniform(-2, 2);
  //LIKELIHOOD
  Y ~ DSjP(p);
}
