context("Testa as Funções de Log-Verossimilhança")

test_that("Tem os mesmo argumentos", {
    apropos("formals")
    a_gcnt <- as.list(formals(llgcnt))
    a_pgnz <- as.list(formals(llpgnz))
    expect_equal(a_gcnt, a_pgnz)
})

test_that("São iguais a Poisson", {
    y <- 0:10
    X <- cbind(y * 0 + 1)
    lamb <- 10 * runif(1)
    ll_pois <- -sum(dpois(x = y, lambda = lamb, log = TRUE))
    ll_pgnz <- llpgnz(params = c(0, log(lamb)), y = y, X = X)
    ll_gcnt <- llgcnt(params = c(log(1), log(lamb)), y = y, X = X)
    expect_equal(ll_pgnz, expected = ll_pois)
    expect_equal(ll_gcnt, expected = ll_pois)
})

test_that("Inteiros negativos resultam em Warning", {
    y <- -4:-1
    X <- cbind(y * 0 + 1)
    lamb <- 10 * runif(1)
    expect_warning(llpgnz(params = c(0, log(lamb)), y = y, X = X))
    expect_warning(llgcnt(params = c(log(1), log(lamb)), y = y, X = X))
})
