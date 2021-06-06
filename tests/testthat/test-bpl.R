test_that("simple test", {
  n.alg  <- 5
  n.inst <- 25
  x.matrix <- matrix(runif(n.alg*n.inst), ncol=n.alg)
  colnames(x.matrix) <- paste("Alg", 1:n.alg, sep="")
  rownames(x.matrix) <- paste("Inst", 1:n.inst, sep="")

  res <- bPlackettLuceModel(x.matrix, min=FALSE, nsim=2000, nchains=3)

  #if it works it should not assert any error
})
