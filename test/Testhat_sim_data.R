source('~/Documents/GitHub/Thesis/Misclassification/Misclassification/Main/sim_alternative.R')

library(testthat)


test_n.cov = 1
test_n.sites = 10
test_n.visit = 2 
test_n.species = 3

test_beta <- matrix(c(1,-2,
                      -2.5, 1.5,
                      1, -1),
                    nrow= test_n.species, 
                    ncol=2, byrow = TRUE)

simulate <- simrep(test_n.species, 
                   test_n.sites, 
                   test_n.visit, 
                   test_n.cov,
                   p.tag=0.5, 
                   test_beta)

test_that("simulation of covariates", {
  expect_equal(dim(simulate[[4]]), c(test_n.sites,test_n.cov))
  #expect_true(all(simulate[[4]][1,,] %in% c("1"))) #Uncomment this if I use X\beta
})

test_that("misclassification probability", {
  expect_true(all(simulate[[3]] >0 & simulate[[3]] < 1))
      expect_true(all(rowSums(simulate[[3]])%in% c("1"))) 

})

test_that("Simulation of C and Y", {
expect_true(all(simulate[[1]]%in% c("0", "1")))
expect_true(all(!is.na(simulate[[2]])%in% c("1", "2", "3", "4")))
})
