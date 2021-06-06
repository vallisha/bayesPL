#' bpl function
#'
#' @param data input data frame with column y
#' @export
#'
#' @examples
bpl <- function(x.matrix,  min=TRUE, prior=rep(1, ncol(x.matrix)),
                nsim=2000, nchains=8,
                seed=123, ...){

  if (length(prior)!=ncol(x.matrix)) {
    stop("The length of the prior vector has to be equal to the number of colums of x.matrix.")
  }

  # Check the input data (we need a matrix or a data.frame)
  if (class(x.matrix) == "integer"){
    x.matrix <- matrix(x.matrix, nrow=1)
  }

  # Create the ranking matrix

  if (min) {
    aux <- x.matrix
  }else{
    aux <- -1*x.matrix
  }

  ranking.matrix <- t(apply(aux, MARGIN=1,
                            FUN=function(i)
                            {
                              r <- rank(i, ties.method='random')
                              return(r)
                            }))

  colnames(ranking.matrix) <- colnames(x.matrix)
  rownames(ranking.matrix) <- rownames(x.matrix)

  # Function to sample the posterior distribution of weights
  data <- list()
  data$n       <- nrow(ranking.matrix)
  data$m       <- ncol(ranking.matrix)
  data$ranks   <- ranking.matrix
  data$alpha   <- prior
  data$weights <- rep(1, data$n)

  model_file <-
    system.file("stan",
                "test.stan",
                package = "bayesPL",
                mustWork = TRUE)

  model <- cmdstanr::cmdstan_model(model_file)

  fit <- model$sample(data=data,
                      chains=nchains,
                      iter=nsim,
                      iter_warmup=500,
                      seed=seed)

  # this is a draws to extract posterior
  draws <- fit$draws()
  # convert to matrix
  posterior <- posterior::as_draws_matrix(fit$draws("ratings"))
  #print summary of the model
  print(fit$summary(NULL, c("mean","median","sd","mad")))

  colnames(posterior) <- colnames(ranking.matrix)

  #apply() takes matrix as an input and gives output in vector, list or array.
  posterior.calculator <- t(apply(posterior, MARGIN=1,
                            FUN=function(i) {
                              return(rank(-i))
                            }))
  #Computation of rank based on the mean of the posterior.
  mean.rank <- colMeans(posterior.calculator)
  names(mean.rank) <- colnames(ranking.matrix)

  #Computation of rank based on the median of the posterior.
  median.rank <- apply(posterior, MARGIN = 2, median)
  names(median.rank) <- colnames(ranking.matrix)

  #Computation of rank based on the SD of the posterior.
  sd.rank <- GMCM:::colSds(posterior)
  names(sd.rank) <- colnames(ranking.matrix)


  parameters <- list(prior=prior, nchains=nchains, nsim=nsim)

  #Printing the results
  results <- list()
  results$method                  <- "Bayesian Plackett-Luce model"
  results$parameters              <- parameters
  results$posterior.weights       <- posterior
  results$title.mean.rank         <- "Rank based on mean of the posterior"
  results$mean.rank               <- mean.rank
  results$title.median.rank       <- "Rank based on median of the posterior"
  results$median.rank             <- median.rank
  results$title.sd.rank         <- "Rank based on SD of the posterior"
  results$sd.rank               <- sd.rank
  results$additional              <- draws

  print(results)

}

