context("Testa as Funções de Probabilidade")

test_that("Iguais a Poisson", {
    y <- 0:30
    lambda <- 30 * runif(1)
    py_pois <- dpois(x = y, lambda = lambda)
    py_pgnz <- dpgnz(y = y, lambda = lambda, alpha = 0)
    py_gcnt <- dgcnt(y = y, lambda = lambda, alpha = 1)
    expect_equal(py_pgnz, py_pois)
    expect_equal(py_gcnt, py_pois)
})
