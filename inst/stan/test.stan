//

//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
    int<lower=1> n; // number of instances
    int<lower=2> m; // number of criterias

    // Matrix with all the rankings, one per row
    int ranks [n,m];

    real weights[n];

    // Parameters for Dirichlet prior.
    vector[m] alpha;
}

transformed data {
  // The implementation of the probability of the PL model uses the order, rather
  // than the rank
  int order [n,m];
  for (i in 1:n){
    for (j in 1:m){
      order[i, ranks[i, j]]=j;
    }
  }
}

parameters {
    // Vector of ratings for each team.
    // The simplex constrains the ratings to sum to 1
    simplex[m] ratings;
}

transformed parameters{
  real loglikelihood;
  real rest;

  loglikelihood=0;
  for (i in 1:n){
    for (j in 1:(m-1)){
      rest=0;
      for (x in j:m){
        rest = rest + ratings[order[i, x]];
      }
      loglikelihood = loglikelihood + log(weights[i] * ratings[order[i, j]] / rest);
    }
  }
}

model {
    ratings ~ dirichlet(alpha); // Dirichlet prior
    target += loglikelihood;
}

