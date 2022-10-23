//
// This Stan program defines a regression ASN model, with a
//

functions{
  real RL_log(vector x, vector p){
    vector[num_elements(x)] prob;
    real lprob;
    for (i in 1:num_elements(x)){
      prob[i] = (p[i]^x[i])*(1-p[i])^(1-x[i]);
    }
    lprob = sum(log(prob));
    return lprob;
  }
}

// The input data is a vector 'y' of length 'N'.
data {
  int <lower=0> N;
  vector[N] Y;
  vector[N] X1; 
  vector[N] X2;
  vector[N] X3;
  vector[N] X4;
  vector[N] X5;
  vector[N] X6;
}

// The parameters accepted by the model. Our model
// accepts five parameters 'beta0', 'beta1', 'beta2', 
// 'beta3', 'sigma' and 'alpha'.
parameters {
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real beta6;
}

// The adopted transformation in the location parameter 'mu'
transformed parameters {
vector[N] p;
for(i in 1:N){
  
  p[i]=(exp(beta0 + beta1*X1[i] + beta2*X2[i] +
  beta3*X3[i] + beta4*X4[i] + beta5*X5[i] +
  beta6*X6[i]))/(1+exp((beta0 + beta1*X1[i] + beta2*X2[i] +
  beta3*X3[i] + beta4*X4[i] + beta5*X5[i] +
  beta6*X6[i])));
}
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu',
// standard deviation 'sigma' and asymmetry 'alpha'.
model {
  //PRIORI
  beta0 ~ normal(0,2.5);
  beta1 ~ normal(0,2.5);
  beta2 ~ normal(0,2.5);
  beta3 ~ normal(0,2.5);
  beta4 ~ normal(0,2.5);
  beta5 ~ normal(0,2.5);
  beta6 ~ normal(0,2.5);
  //LIKELIHOOD
  Y ~ RL(p);
}

