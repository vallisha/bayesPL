install.packages("bayesplot")# Instructions


1. use the package devtools to add new files etc
2. use roxygen2 to document your functions
  * when you create a new function you go to Code>Insert roxygen skeleton
  * after you run devtools::document() so it updates the documentation automatically
3. Everytime you modify the package you need to reload it
  * devtools::load_all()
5. cmdstanr is used in this package.
6. Every new R file must be in R folder
7. Stan files go to inst/stan
8. test files go in tests. Just follow the examples or use the function use_test()
9. You need to have cmdstanr installed. Check their webpage https://github.com/stan-dev/cmdstanr
