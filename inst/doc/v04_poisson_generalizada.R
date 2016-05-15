## ----setup, include=FALSE-----------------------------------------
source("_setup.R")

## ---- message=FALSE, error=FALSE, warning=FALSE-------------------
# Definições da sessão.
# devtools::load_all("../")
library(lattice)
library(latticeExtra)
library(rpanel)
library(bbmle)
library(corrplot)
library(plyr)
library(car)
library(doBy)
library(multcomp)
library(MRDCr)

## -----------------------------------------------------------------
# Função densidade na parametrização original.
dpgnz0 <- function(y, theta, gamma, m = 4) {
    if (gamma < 0) {
        m <- max(c(m, floor(-theta/gamma)))
        if (gamma < max(c(-1, -theta/m))) {
            m <- 0
        } else {
            m <- as.integer(y <= m)
        }
    } else {
        m <- 1
    }
    z <- theta + gamma * y
    k <- lfactorial(y)
    # fy <- m * theta * z^(y - 1) * exp(-z)/exp(k)
    fy <- m * exp(log(theta) + (y - 1) * log(z) - z - k)
    return(fy)
}

# Caso Poisson (gamma == 0).
y <- 0:30
theta <- 10
gamma <- 0

fy <- dpgnz0(y = y, theta = theta, gamma = gamma)
plot(fy ~ y, type = "h", xlab = "y", ylab = "f(y)")
lines(y + 0.3, dpois(y, lambda = theta), type = "h", col = 2)

## ---- eval=FALSE--------------------------------------------------
#  react <- function(panel){
#      with(panel,
#      {
#          m <- THETA/(1 - GAMMA)
#          s <- sqrt(THETA/(1 - GAMMA)^3)
#          from <- floor(max(c(0, m - 5 * s)))
#          to <- ceiling(max(c(YMAX, m + 5 * s)))
#          y <- from:to
#          py <- dpgnz0(y = y, theta = THETA, gamma = GAMMA)
#          if (POIS) {
#              pz <- dpois(y, lambda = m)
#          } else {
#              pz <- 0
#          }
#          # Colocar 0 para valores não finitos (-Inf, Inf e NaN) para
#          # fazer gráfico.
#          py[!is.finite(py)] <- 0
#          plot(py ~ y, type = "h",
#               ylim = c(0, max(c(py, pz))),
#               xlab = expression(y),
#               ylab = expression(f(y)))
#          mtext(side = 3,
#                text = substitute(sum(f(y)) == s,
#                                  list(s = round(sum(py), 5))))
#          if (EX) {
#              abline(v = m, col = 2)
#          }
#          if (POIS) {
#              lines(y + 0.3, pz, type = "h", col = 3)
#          }
#      })
#      panel
#  }
#  
#  panel <- rp.control(title = "Poisson Generalizada",
#                      size = c(300, 100), YMAX = 150, m = 4)
#  rp.slider(panel = panel, action = react,
#            variable = THETA, title = "theta",
#            from = 0.1, to = 100,
#            initval = 5, resolution = 0.1,
#            showvalue = TRUE)
#  rp.slider(panel = panel, action = react,
#            variable = GAMMA, title = "gamma",
#            from = -1, to = 0.99,
#            initval = 0, resolution = 0.01,
#            showvalue = TRUE)
#  rp.checkbox(panel = panel,
#              variable = EX, action = react, title = "E(Y)",
#              labels = "Mostrar o valor esperado?")
#  rp.checkbox(panel = panel,
#              variable = POIS, action = react, title = "Poisson",
#              labels = "Adicionar a distribução Poisson?")
#  rp.do(panel = panel, action = react)

## ---- eval=FALSE--------------------------------------------------
#  # Função densidade na parametrização de modelo de regressão.
#  MRDCr::dpgnz
#  
#  react <- function(panel){
#      with(panel,
#      {
#          m <- LAMBDA
#          s <- sqrt(LAMBDA) * (1 + ALPHA * LAMBDA)
#          from <- floor(max(c(0, m - 5 * s)))
#          to <- ceiling(max(c(YMAX, m + 5 * s)))
#          y <- from:to
#          py <- dpgnz(y = y, lambda = LAMBDA, alpha = ALPHA)
#          if (POIS) {
#              pz <- dpois(y, lambda = m)
#          } else {
#              pz <- 0
#          }
#          py[!is.finite(py)] <- 0
#          plot(py ~ y, type = "h",
#               ylim = c(0, max(c(py, pz))),
#                   xlab = expression(y),
#               ylab = expression(f(y)))
#          mtext(side = 3,
#                text = substitute(sum(f(y)) == s,
#                                  list(s = round(sum(py), 5))))
#          if (EX) {
#              abline(v = m, col = 2)
#          }
#          if (POIS) {
#              lines(y + 0.3, pz, type = "h", col = 3)
#          }
#      })
#      panel
#  }
#  
#  panel <- rp.control(title = "Poisson Generalizada",
#                      size = c(300, 100), YMAX = 150)
#  rp.slider(panel = panel, action = react,
#            variable = LAMBDA, title = "lambda",
#            from = 0.1, to = 100,
#            initval = 5, resolution = 0.1,
#            showvalue = TRUE)
#  rp.slider(panel = panel, action = react,
#            variable = ALPHA, title = "alpha",
#            from = -0.1, to = 0.4,
#            initval = 0, resolution = 0.01,
#            showvalue = TRUE)
#  rp.checkbox(panel = panel,
#              variable = EX, action = react, title = "E(Y)",
#              labels = "Mostrar o valor esperado?")
#  rp.checkbox(panel = panel,
#              variable = POIS, action = react, title = "Poisson",
#              labels = "Adicionar a distribução Poisson?")
#  rp.do(panel = panel, action = react)

## -----------------------------------------------------------------
#-----------------------------------------------------------------------
# Gráfico do espaço paramétrico de theta x gamma.

fun <- Vectorize(vectorize.args = c("theta", "gamma"),
                 FUN = function(theta, gamma) {
                     sum(dpgnz0(y = y, theta = theta, gamma = gamma))
                 })

grid <- list(theta = seq(1, 50, by = 1),
             gamma = seq(-0.5, 1, by = 0.05))
str(grid)

y <- 0:500
my <- max(y)

grid$sum <- with(grid, outer(theta, gamma, fun))
grid <- with(grid,
             cbind(expand.grid(theta = theta, gamma = gamma),
                   data.frame(sum = c(sum))))

levelplot(sum ~ theta + gamma,
          data = subset(grid, round(sum, 3) == 1),
          col.regions = gray.colors) +
    layer(panel.abline(a = 0, b = -1/my)) +
    layer(panel.abline(h = 0, lty = 2))

#-----------------------------------------------------------------------
# Gráfico do espaço paramétrico de lambda x alpha.

fun <- Vectorize(vectorize.args = c("lambda", "alpha"),
                 FUN = function(lambda, alpha) {
                     sum(dpgnz(y = y, lambda = lambda, alpha = alpha))
                 })

grid <- list(lambda = seq(0.2, 50, by = 0.2),
             alpha = seq(-0.98, 0.98, by = 0.02))
grid$sum <- with(grid, outer(lambda, alpha, fun))

grid <- with(grid,
             cbind(expand.grid(lambda = lambda, alpha = alpha),
                   data.frame(sum = c(sum))))

levelplot(sum ~ lambda + alpha,
          data = subset(grid, round(sum, 3) == 1),
          col.regions = gray.colors) +
    layer(panel.abline(h = 0, lty = 2)) +
    layer(panel.curve(-1/x))

# Já que lambda * alpha > -1, então alpha = -1/lambda dá a fronteira.

## -----------------------------------------------------------------
# Função de log-Verossimilhança da Poisson Generalizada na
# parametrização de modelo de regressão.
MRDCr::llpgnz

#-----------------------------------------------------------------------
# Gerando uma amostra aleatória da Poisson, para teste.

# Offset = 2, lambda = 3.
y <- rpois(100, lambda = 2 * 3)

L <- list(y = y,
          offset = rep(2, length(y)),
          X = cbind(rep(1, length(y))))

start <- c(alpha = 0, lambda = 1)
parnames(llpgnz) <- names(start)

# Como \alpha foi fixado em 1, essa ll corresponde à Poisson.
n0 <- mle2(minuslogl = llpgnz,
           start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Para conferir.
c(coef(n0)["lambda"],
  coef(glm(y ~ offset(log(L$offset)), family = poisson)))

# Estimando o \alpha.
n1 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)
coef(n1)

# Perfil de verossimilhança dos parâmetros.
plot(profile(n1))

# Covariância.
V <- cov2cor(vcov(n1))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## -----------------------------------------------------------------
#-----------------------------------------------------------------------
# Carregando e explorando os dados.

data(soja, package = "MRDCr")
str(soja)

# A observação 74 é um outlier.
soja <- soja[-74, ]

xyplot(nvag ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       ylab = "Número de vagens por parcela",
       xlab = expression("Dose de potássio aplicada"~(mg ~ dm^{3})),
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade"))

soja <- transform(soja, K = factor(K))

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(nvag ~ bloc + umid * K, data = soja, family = poisson)
m1 <- update(m0, family = quasipoisson)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0); layout(1)

# Medidas de decisão.
# anova(m0, test = "Chisq")
anova(m1, test = "F")
summary(m1)

#-----------------------------------------------------------------------
# Modelo Poisson Generalizado.

L <- with(soja,
          list(y = nvag, offset = 1, X = model.matrix(m0)))

# Usa as estimativas do Poisson como valores iniciais para a PGen.
start <- c(alpha = 0, coef(m0))
parnames(llpgnz) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))
cbind(coef(m2)[-1], coef(m0))

# Poisson Generalizada.
m3 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

# Estimativas dos coeficientes.
cbind("PoissonGLM" = c(NA, coef(m0)),
      "PoissonML" = coef(m2),
      "PGeneraliz" = coef(m3))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))
abline(v = 0, lty = 2)

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

## -----------------------------------------------------------------
#-----------------------------------------------------------------------
# Testes de hipótese.

# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m3) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Cáclculo da estatística Chi-quadrado.
# t(L %*% coef(m3)) %*%
#     solve(L %*% vcov(m3) %*% t(L)) %*%
#     (L %*% coef(m3))
crossprod(L %*% coef(m3),
          solve(L %*% vcov(m3) %*% t(L),
                L %*% coef(m3)))

# Teste de Wald para interação (poderia ser LRT, claro).
# É necessário passar um objeto glm mesmo fornecendo o restante a parte.
linearHypothesis(model = m0,
                 hypothesis.matrix = L,
                 vcov. = vcov(m3),
                 coef. = coef(m3))

#-----------------------------------------------------------------------
# Predição com bandas de confiança.

X <- LSmatrix(m0, effect = c("umid", "K"))

pred <- attr(X, "grid")
pred <- transform(pred,
                  K = as.integer(K),
                  umid = factor(umid))
pred <- list(pois = pred, pgen = pred)

# Quantil normal.
qn <- qnorm(0.975) * c(lwr = -1, fit = 0, upr = 1)

# Preditos pela Poisson.
# aux <- predict(m0, newdata = pred$pois, se.fit = TRUE)
# aux <- exp(aux$fit + outer(aux$se.fit, qn, FUN = "*"))
# pred$pois <- cbind(pred$pois, aux)
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))
str(pred$pois)

# Matrix de covariância completa e sem o alpha (marginal).
V <- vcov(m3)
V <- V[-1, -1]
U <- chol(V)
aux <- sqrt(apply(X %*% t(U), MARGIN = 1,
                  FUN = function(x) { sum(x^2) }))
pred$pgen$eta <- c(X %*% coef(m3)[-1])
pred$pgen <- cbind(pred$pgen,
                   apply(outer(aux, qn, FUN = "*"), MARGIN = 2,
                         FUN = function(x) {
                             exp(pred$pgen$eta + x)
                         }))

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Poisson Generelizada")))

xyplot(fit ~ K | umid, data = pred,
       layout = c(NA, 1), as.table = TRUE,
       xlim = extendrange(range(pred$K), f = 0.1),
       key = key, pch = pred$modelo,
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de vagens por parcela",
       ly = pred$lwr, uy = pred$upr, cty = "bars", length = 0,
       prepanel = prepanel.cbH,
       desloc = 8 * scale(as.integer(pred$modelo), scale = FALSE),
       panel = panel.cbH)

## -----------------------------------------------------------------
#-----------------------------------------------------------------------

xyplot(ngra ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       ylab = "Número de grãos por parcela",
       xlab = expression("Dose de potássio aplicada"~(mg ~ dm^{3})),
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade"))

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(ngra ~ bloc + umid * K, data = soja, family = poisson)
m1 <- update(m0, family = quasipoisson)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0); layout(1)

# Medidas de decisão.
# anova(m0, test = "Chisq")
anova(m1, test = "Chisq")
summary(m1)

#-----------------------------------------------------------------------
# Modelo Poisson Generalizado.

L <- with(soja,
          list(y = ngra, offset = 1, X = model.matrix(m0)))

# Usa as estimativas do Poisson como valores iniciais.
start <- c(alpha = 0, coef(m0))
parnames(llpgnz) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

# Estimaitvas dos parâmetros.
cbind("PoissonGLM" = c(NA, coef(m0)),
      "PoissonML" = coef(m2),
      "PGeneraliz" = coef(m3))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))
abline(v = 0, lty = 2)

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

## -----------------------------------------------------------------
# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m3) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Cáclculo da estatística Chi-quadrado.
crossprod(L %*% coef(m3),
          solve(L %*% vcov(m3) %*% t(L),
                L %*% coef(m3)))

linearHypothesis(model = m0,
                 hypothesis.matrix = L,
                 vcov. = vcov(m3),
                 coef. = coef(m3))

#-----------------------------------------------------------------------
# Predição com bandas de confiança.

X <- LSmatrix(m0, effect = c("umid", "K"))

pred <- attr(X, "grid")
pred <- transform(pred,
                  K = as.integer(K),
                  umid = factor(umid))
pred <- list(pois = pred, quasi = pred, pgen = pred)

# Quantil normal.
qn <- qnorm(0.975) * c(lwr = -1, fit = 0, upr = 1)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m1, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Poisson Generalizada.
V <- vcov(m3)
V <- V[-1, -1]
U <- chol(V)
aux <- sqrt(apply(X %*% t(U), MARGIN = 1,
                  FUN = function(x) { sum(x^2) }))
pred$pgen$eta <- c(X %*% coef(m3)[-1])
pred$pgen <- cbind(pred$pgen,
                   apply(outer(aux, qn, FUN = "*"), MARGIN = 2,
                         FUN = function(x) {
                             exp(pred$pgen$eta + x)
                         }))

# Junta o resultado dos 3 modelos.
pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)
str(pred)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Poisson Generelizada")))

xyplot(fit ~ K | umid, data = pred,
       layout = c(NA, 1), as.table = TRUE,
       xlim = extendrange(range(pred$K), f = 0.1),
       key = key, pch = pred$modelo,
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de grãos por parcela",
       ly = pred$lwr, uy = pred$upr, cty = "bars", length = 0,
       prepanel = prepanel.cbH,
       desloc = 8 * scale(as.integer(pred$modelo), scale = FALSE),
       panel = panel.cbH)

## -----------------------------------------------------------------
#-----------------------------------------------------------------------
# Número de grãos por vagem (offset).

xyplot(ngra/nvag ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de grãos por vagem",
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade"))

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(ngra ~ offset(log(nvag)) + bloc + umid * K,
          data = soja, family = poisson)
m1 <- update(m0, family = quasipoisson)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0); layout(1)

# Medidas de decisão.
# anova(m0, test = "Chisq")
anova(m1, test = "F")
summary(m1)

# Declara um modelo aditivo.
m0 <- glm(ngra ~ offset(log(nvag)) + bloc + umid + K,
          data = soja, family = poisson)
m1 <- update(m0, family = quasipoisson)
anova(m1, test = "F")

#-----------------------------------------------------------------------
# Modelo Poisson Generalizado.

L <- with(soja,
          list(y = ngra, offset = nvag, X = model.matrix(m0)))

# De acordo com a estimativa de phi da Quasi-Poisson, esse dado é
# subdisperso. Já que na verossimilhaça (1 + alpha * y) > -1 quando
# alpha < 0, então o menor valor possível para gamma para ter uma
# log-verossimilhança avaliável é
-1/max(soja$ngra)

# Mesmo com esse lower bound, o valor chute para alpha foi definido por
# tentativa erro. O valor -0.003 dá Error e o valor -0.002 na hora de
# perfilhar encontra um mínimo melhor. Então, por tentativa erro
# chegou-se no -0.0026.
start <- c(alpha = -0.0026, coef(m0))
parnames(llpgnz) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

cbind("PoissonGLM" = c(NA, coef(m0)),
      "PoissonML" = coef(m2),
      "PGeneraliz" = coef(m3))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## -----------------------------------------------------------------
# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m3) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Cáclculo da estatística Chi-quadrado.
crossprod(L %*% coef(m3),
          solve(L %*% vcov(m3) %*% t(L),
                L %*% coef(m3)))

linearHypothesis(model = m0,
                 hypothesis.matrix = L,
                 vcov. = vcov(m3),
                 coef. = coef(m3))

#-----------------------------------------------------------------------
# Predição com bandas de confiança.

# Por causa do offset, não tem como usar a LSmatrix.
pred <- unique(subset(soja, select = c("umid", "K")))

X <- model.matrix(formula(m0)[-2],
                  data = cbind(nvag = 1, bloc = soja$bloc[1], pred))

i <- grep(x = colnames(X), pattern = "^bloc")
X[, i] <- X[, i] * 0 + 1/(length(i) + 1)
head(X)

pred <- list(pois = pred, quasi = pred, pgen = pred)

# Quantil normal.
qn <- qnorm(0.975) * c(lwr = -1, fit = 0, upr = 1)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m1, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Poisson Generalizada.
V <- vcov(m3)
V <- V[-1, -1]
U <- chol(V)
aux <- sqrt(apply(X %*% t(U), MARGIN = 1,
                  FUN = function(x) { sum(x^2) }))
pred$pgen$eta <- c(X %*% coef(m3)[-1])
pred$pgen <- cbind(pred$pgen,
                   apply(outer(aux, qn, FUN = "*"), MARGIN = 2,
                         FUN = function(x) {
                             exp(pred$pgen$eta + x)
                         }))

# Junta o resultado dos 3 modelos.
pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)
pred$K <- as.numeric(as.character(pred$K))

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Poisson Generelizada")))

xyplot(fit ~ K | umid, data = pred,
       layout = c(NA, 1), as.table = TRUE,
       xlim = extendrange(range(pred$K), f = 0.2),
       key = key, pch = pred$modelo,
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de grãos por parcela",
       ly = pred$lwr, uy = pred$upr, cty = "bars", length = 0,
       prepanel = prepanel.cbH,
       desloc = 8 * scale(as.integer(pred$modelo), scale = FALSE),
       panel = panel.cbH)

## -----------------------------------------------------------------
#-----------------------------------------------------------------------
# Número de capulhos em função do nível de desfolha artificial e fase
# fenológica do algodoeiro.

data(capdesfo, package = "MRDCr")
str(capdesfo)

p1 <- xyplot(ncap ~ des | est, data = capdesfo,
             col = 1, type = c("p", "smooth"), col.line = "gray50",
             layout = c(2, 3), as.table = TRUE,
             xlim = extendrange(c(0:1), f = 0.15),
             xlab = "Nível de desfolhas artificial",
             ylab = "Número de capulho produzidos",
             spread = 0.035, panel = panel.beeswarm)
p1

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(ncap ~ est * (des + I(des^2)),
          data = capdesfo, family = poisson)
m1 <- update(m0, family = quasipoisson)

par(mfrow = c(2, 2))
plot(m0); layout(1)

anova(m0, test = "Chisq")
anova(m1, test = "F")
summary(m1)

#-----------------------------------------------------------------------
# Modelo Poisson Generalizada.

L <- with(capdesfo,
          list(y = ncap, offset = 1, X = model.matrix(m0)))

start <- c(alpha = log(1), coef(m0))
parnames(llpgnz) <- names(start)

# Modelo Poisson também.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

c(logLik(m2), logLik(m0))

# Modelo Poisson Generalizado.
m3 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)
logLik(m3)

anova(m3, m2)

summary(m3)

plot(profile(m3, which = "alpha"))

cbind("PoissonGLM" = c(NA, coef(m0)),
      "PoissonML" = coef(m2),
      "PGeneraliz" = coef(m3))

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## -----------------------------------------------------------------
# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m3) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Teste de Wald explicito.
crossprod(L %*% coef(m3),
          solve(L %*% vcov(m3) %*% t(L),
                L %*% coef(m3)))

# Teste de Wald para interação (poderia ser LRT, claro).
# É necessário um objeto glm, mas necesse caso ele não usado para nada.
linearHypothesis(model = m0,
                 hypothesis.matrix = L,
                 vcov. = vcov(m3),
                 coef. = coef(m3))

#-----------------------------------------------------------------------
# Predição com bandas de confiança.

pred <- with(capdesfo, expand.grid(est = levels(est),
                                   des = seq(0, 1, by = 0.025)))
X <- model.matrix(formula(m0)[-2], data = pred)
pred <- list(pois = pred, quasi = pred, pgen = pred)

# Quantil normal.
qn <- qnorm(0.975) * c(lwr = -1, fit = 0, upr = 1)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m1, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Poisson Generalizada.
V <- vcov(m3)
V <- V[-1, -1]
U <- chol(V)
aux <- sqrt(apply(X %*% t(U), MARGIN = 1,
                  FUN = function(x) { sum(x^2) }))
pred$pgen$eta <- c(X %*% coef(m3)[-1])
pred$pgen <- cbind(pred$pgen,
                   apply(outer(aux, qn, FUN = "*"), MARGIN = 2,
                         FUN = function(x) {
                             exp(pred$pgen$eta + x)
                         }))

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, est, des, modelo)

key <- list(lines = list(lty = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Poisson Generelizada")))
key$lines$col <-
    trellis.par.get("superpose.line")$col[1:nlevels(pred$modelo)]

p2 <- xyplot(fit ~ des | est, data = pred, groups = modelo,
             layout = c(NA, 1), as.table = TRUE,
             xlim = extendrange(range(pred$des), f = 0.1),
             type = "l", key = key,
             ly = pred$lwr, uy = pred$upr,
             cty = "bands", alpha = 0.35,
             prepanel = prepanel.cbH,
             panel.groups = panel.cbH,
             panel = panel.superpose)
# p2

## ---- fig.width=7, fig.height=3.5---------------------------------
update(p1, type = "p", layout = c(NA, 1),
       key = key, spread = 0.07) +
    as.layer(p2, under = TRUE)

## -----------------------------------------------------------------
#-----------------------------------------------------------------------

data(nematoide, package = "MRDCr")
str(nematoide)

# Número de nematóides por grama de raíz.
plot(nema ~ off, data = nematoide)

# Média do número de nematóides por grama de raíz.
mv <- aggregate(cbind(y = nema/off) ~ cult, data = nematoide,
                FUN = function(x) c(m = mean(x), v = var(x)))

xyplot(y[, "v"] ~ y[, "m"], data = mv,
       xlab = "Média amostral",
       ylab = "Variância amostral") +
    layer(panel.abline(a = 0, b = 1, lty = 2))

#-----------------------------------------------------------------------
# Ajuste do Poisson.

m0 <- glm(nema ~ offset(log(off)) + cult,
          data = nematoide,
          family = poisson)
m1 <- update(m0, family = quasipoisson)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0); layout(1)

# Estimativas dos parâmetros.
summary(m1)

# Quadro de deviance.
# anova(m0, test = "Chisq")
anova(m1, test = "F")

#-----------------------------------------------------------------------
# Ajuste da Poisson Generalizada.

L <- with(nematoide,
          list(y = nema, offset = off, X = model.matrix(m0)))

start <- c(alpha = log(1), coef(m0))
parnames(llpgnz) <- names(start)

# Modelo Poisson também.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- pgnz(formula(m0), data = nematoide)

# Diferença de deviance.
# 2 * diff(c(logLik(m0), logLik(m3)))
anova(m3, m2)

# Perfil de log-verossimilhança para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))

# Covariância.
V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## -----------------------------------------------------------------
# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

# Gráfico das estimativas.
pars <- data.frame(Pois = c(0, coef(m0)), PGen = coef(m3))
xyplot(PGen ~ Pois, data = pars, aspect = "iso", grid = TRUE) +
    layer(panel.abline(a = 0, b = 1, lty = 2))

#-----------------------------------------------------------------------

X <- model.matrix(m0)

# # Predito do número de nematóides observado (considera o offset).
# with(nematoide, {
#     cbind(y = nema,
#           Pois = nematoide$off * exp(X %*% coef(m0)),
#           PGen = nematoide$off * exp(X %*% coef(m1)[-1]))
# })

# Predito do número de nematóides por grama de raíz.
pred <- with(nematoide, {
    data.frame(y = nema/off,
               Pois = c(exp(X %*% coef(m0))),
               PGen = c(exp(X %*% coef(m3)[-1])))
})
str(pred)

splom(pred) + layer(panel.abline(a = 0, b = 1))

# Correlação predito x observado.
cor(pred)

# Média das observações de das estimativas por cultivar.
predm <- aggregate(as.matrix(pred) ~ cult, data = nematoide, FUN = mean)
cor(predm[, -1])

#-----------------------------------------------------------------------
# Predição com intervalos de confiança.

pred <- unique(subset(nematoide, select = cult))
X <- model.matrix(~cult, data = pred)

pred <- list(pois = pred, quasi = pred, pgen = pred)

# Quantil normal.
qn <- qnorm(0.975) * c(lwr = -1, fit = 0, upr = 1)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m1, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Poisson Generalizada.
V <- vcov(m3)
V <- V[-1, -1]
U <- chol(V)
aux <- sqrt(apply(X %*% t(U), MARGIN = 1,
                  FUN = function(x) { sum(x^2) }))
pred$pgen$eta <- c(X %*% coef(m3)[-1])
pred$pgen <- cbind(pred$pgen,
                   apply(outer(aux, qn, FUN = "*"), MARGIN = 2,
                         FUN = function(x) {
                             exp(pred$pgen$eta + x)
                         }))

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, cult, modelo)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Poisson Generelizada")))

xyplot(nema/off ~ cult, data = nematoide,
       key = key,
       xlab = "Cultivar de feijão",
       ylab = "Número de nematóides por grama de raíz") +
    as.layer(
        xyplot(fit ~ cult, data = pred,
               pch = pred$modelo,
               ly = pred$lwr, uy = pred$upr,
               cty = "bars", length = 0,
               prepanel = prepanel.cbH,
               desloc = 0.25 * scale(as.integer(pred$modelo),
                                    scale = FALSE),
               panel = panel.cbH))

#-----------------------------------------------------------------------
# Resíduos de Pearson.

X <- model.matrix(m0)

# # Resíduos de Pearson no Poisson.
# with(nematoide,  {
#     y <- nema
#     # haty <- fitted(m0)
#     haty <- nematoide$off * exp(X %*% coef(m0))
#     sdy <- sqrt(haty)
#     cbind((y - haty)/sdy,
#           residuals(m0, type = "pearson"))
# })

# Resíduos de Pearson do Poisson Generalizado.
rp <- with(nematoide,  {
    y <- nema
    alph <- coef(m3)["alpha"]
    haty <- c(nematoide$off * exp(X %*% coef(m3)[-1]))
    sdy <- sqrt(haty) * (1 + alph * haty)
    (y - haty)/sdy
})

rp <- stack(data.frame(Pois = residuals(m0, type = "pearson"),
                       PGen = rp))

qqmath(~values | ind, data = rp,
       xlab = "Quantis teóricos",
       ylab = "Resíduos de Pearson",
       panel = function(...) {
           panel.qqmathline(...)
           panel.qqmath(...)
       })


