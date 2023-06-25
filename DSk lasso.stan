functions{
  real DSk_log(vector x, vector p){
    vector[num_elements(x)] prob;
    real lprob;
    for (i in 1:num_elements(x)){
      prob[i] = p[i]^x[i]*(1-p[i])^(1-x[i]);
    }
    lprob = sum(log(prob));
    return lprob;
  }
  
  int signnum(real x) { return x < 0 ? -1 : x > 0; } 
}

data {
  int <lower=0> N;
  vector[N] Y;
  matrix[N, 6] X;
  real <lower=0> tau; // hiperparâmetro Lasso
}

parameters {
  vector[7] beta;
}

transformed parameters {
  vector[N] p;
  for (i in 1:N) {
    real temp = beta[1] + dot_product(X[i], beta[2:]);
    real fda = (1/8) * (exp(temp) * (temp - 2) * (sign(temp) - 1) + exp(-temp) * (-2 + 4*exp(temp) - temp) * (sign(temp) + 1));
    real num = 1.0 - 0.5 * (2 + abs(fda)) * exp(-abs(fda));
    p[i] = 0.5 + 0.5 * signnum(fda) * num;
  }
}

model {
  // priori Lasso: distribuição Laplace para os coeficientes
  for (j in 1:7) {
    beta[j] ~ double_exponential(0, tau);
  }
  
  Y ~ DSk(p);
}