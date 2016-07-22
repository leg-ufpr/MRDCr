## ----setup, include=FALSE------------------------------------------------
source("_setup.R")

## ---- message=FALSE, error=FALSE, warning=FALSE--------------------------
# Definições da sessão.
library(lattice)
library(latticeExtra)
library(grid)
library(bbmle)
library(corrplot)
library(plyr)
library(car)
library(doBy)
library(multcomp)
library(MRDCr)

## ------------------------------------------------------------------------
# Função densidade na parametrização original.
dgcnt

# Caso Poisson (gamma == 0).
grid <- expand.grid(lambda = c(2, 8, 15),
                    alpha = c(0.5, 1, 2.5))
y <- 0:30
py <- mapply(FUN = dgcnt,
             lambda = grid$lambda,
             alpha = grid$alpha,
             MoreArgs = list(y = y), SIMPLIFY = FALSE)
grid <- cbind(grid[rep(1:nrow(grid), each = length(y)), ],
              y = y,
              py = unlist(py))

useOuterStrips(xyplot(py ~ y | factor(lambda) + factor(alpha),
                      data = grid, type = "h",
                      xlab = expression(y),
                      ylab = expression(p(y)),
                      panel = function(x, y, ...) {
                          m <- sum(x * y)
                          panel.xyplot(x, y, ...)
                          panel.abline(v = m, lty = 2)
                      }),
               strip = strip.custom(
                   strip.names = TRUE,
                   var.name = expression(lambda == ""),
                   sep = ""),
               strip.left = strip.custom(
                   strip.names = TRUE,
                   var.name = expression(alpha == ""),
                   sep = ""))

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------
# A média da Gamma-Count.

y <- rpois(100, lambda = 5)
L <- list(y = y, X = cbind(rep(1, length(y))))
start <- c(alpha = 0, lambda = 1)
parnames(llgcnt) <- names(start)
n1 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)

c(mean(y),
  exp(coef(n1)[2]),
  calc_mean_gcnt(lambda = exp(coef(n1)[2]),
                 alpha = exp(coef(n1)[1])))

y <- rpois(500, lambda = 50)
L <- list(y = y, X = cbind(rep(1, length(y))))
start <- c(alpha = 0, lambda = 1)
parnames(llgcnt) <- names(start)
n1 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)

c(mean(y),
  exp(coef(n1)[2]),
  calc_mean_gcnt(lambda = exp(coef(n1)[2]),
               alpha = exp(coef(n1)[1])))

#-----------------------------------------------------------------------
# A E(y) por sum(y * p(py)) e por lambda como função de alpha.

grid <- expand.grid(lambda = exp(seq(-3, 3, length.out = 30)),
                    alpha = exp(seq(-2, 2, length.out = 11)),
                    KEEP.OUT.ATTRS = FALSE)
summary(grid)

y <- 0:500
py <- mapply(FUN = dgcnt,
             lambda = grid$lambda,
             alpha = grid$alpha,
             MoreArgs = list(y = y), SIMPLIFY = FALSE)
grid$m <- sapply(py, FUN = function(py) sum(y * py))
grid$alpha <- round(grid$alpha, 3)
str(grid)

cl <- brewer.pal(n = 10, name = "Spectral")

xyplot(m ~ lambda, groups = alpha, data = grid, type = "l",
       aspect = "iso", grid = TRUE, lwd = 2,
       xlab = expression(lambda),
       ylab = expression(sum(y %.% f(y))),
       auto.key = list(space = "right",
                       title = expression(alpha),
                       points = FALSE, lines = TRUE),
       par.settings = list(superpose.line = list(col = cl))) +
    layer(panel.abline(a = 0, b = 1, lty = 2)) +
    layer(panel.rect(xleft = 0, ybottom = 0,
                     xright = 5, ytop = 5, lty = 2)) +
    layer(panel.arrows(7, 13, 13, 7, length = 0.1)) +
    layer(panel.text(x = 13, y = 7,
                     labels = expression(alpha), pos = 4))

xyplot(m ~ lambda, groups = alpha, type = "l",
       data = subset(grid, findInterval(lambda, vec = c(0, 5)) == 1),
       xlab = expression(lambda),
       ylab = expression(sum(y %.% f(y))),
       aspect = "iso", grid = TRUE, lwd = 2,
       auto.key = list(space = "right",
                       title = expression(alpha),
                       points = FALSE, lines = TRUE),
       par.settings = list(superpose.line = list(col = cl))) +
    layer(panel.abline(a = 0, b = 1, lty = 2)) +
    layer(panel.abline(a = -0.5, b = 1, lty = 2)) +
    layer(panel.abline(a = 1, b = 1, lty = 2))

## ------------------------------------------------------------------------
h <- function(...) {
    dgamma(...)/(1 - pgamma(...))
}

shape <- seq(0.5, 1.5, by = 0.1)

col <- brewer.pal(n = 5, name = "Spectral")
col <- colorRampPalette(colors = col)(length(shape))

curve(dgamma(x, shape = shape[1], rate = 1),
      from = 0, to = 5, col = col[1], lwd = 2,
      xlab = expression(tau),
      ylab = expression(f(tau)))
for (s in 2:length(shape)) {
    curve(dgamma(x, shape = shape[s], rate = 1),
          add = TRUE, col = col[s], lwd = 2)
}
legend("topright", legend = sprintf("%0.3f", shape),
       col = col, lty = 1, lwd = 2, bty = "n",
       title = expression(alpha))

curve(h(x, shape = shape[1], rate = 1),
      from = 0, to = 10, col = col[1], lwd = 2,
      ylim = c(0, 2.5),
      xlab = expression(tau),
      ylab = expression(f(tau)/(1 - F(tau))))
for (s in 2:length(shape)) {
    curve(h(x, shape = shape[s], rate = 1), add = TRUE,
          col = col[s], lwd = 2)
}
legend("topright", legend = sprintf("%0.3f", shape),
       col = col, lty = 1, lwd = 2, bty = "n",
       title = expression(alpha))

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------
# Função de log-Verossimilhança da Poisson Generalizada na
# parametrização de modelo de regressão.

MRDCr::llgcnt

#-----------------------------------------------------------------------
# Gerando uma amostra aleatória da Poisson, para teste.

# Offset = 2, lambda = 3.
y <- rpois(100, lambda = 2 * 3)

L <- list(y = y,
          offset = rep(2, length(y)),
          X = cbind(rep(1, length(y))))

start <- c(alpha = 0, lambda = 1)
parnames(llgcnt) <- names(start)

# Como \alpha foi fixado em 1, essa ll corresponde à Poisson.
n0 <- mle2(minuslogl = llgcnt,
           start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Para conferir.
c(coef(n0)["lambda"],
  coef(glm(y ~ offset(log(L$offset)), family = poisson)))

# Estimando o \alpha.
n1 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)
coef(n1)

# Perfil de verossimilhança dos parâmetros.
plot(profile(n1))

# Covariância.
V <- cov2cor(vcov(n1))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------
# Carregando e explorando os dados.

data(soja, package = "MRDCr")
str(soja)

# help(soja, package = "MRDCr", help_type = "html")

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
parnames(llgcnt) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llgcnt, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))
cbind(coef(m2)[-1], coef(m0))

# Gamma-Count estimando o alpha.
m3 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

# Estimaitvas dos parâmetros.
c0 <- cbind("PoissonGLM" = c(NA, coef(m0)),
            "PoissonML" = coef(m2),
            "GCount" = coef(m3))
c0

splom(c0[-(1:2), ]) + layer(panel.abline(a = 0, b = 1, lty = 2))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))
abline(v = 0, lty = 2)

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

#-----------------------------------------------------------------------
# Testes de hipótese.

# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m3) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Cáclculo da estatística Chi-quadrado.
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
pred <- list(pois = pred, quasi = pred, gcnt = pred)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Gamma-Count.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "link")
pred$gcnt <- cbind(pred$gcnt, exp(aux[, c(2, 1, 3)]))

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Gamma-Count")))

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

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------

xyplot(ngra ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       ylab = "Número de grãos por parcela",
       xlab = expression("Dose de potássio aplicada"~(mg ~ dm^{3})),
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade"))

# NOTE: Essa contagem é alta e uma análise preliminar não retornou
# Hessiana para o modelo Gamma-Count ajustado com a mle2. A suspeita que
# é seja pela ordem de magnitude dos dados. Sendo assim, vamos
# considerar um offset artifical de 10 apenas para correr as análises.
#
# Warning message:
# In mle2(llgcnt, start = start, data = L, fixed = list(alpha = 0),  :
#   couldn't invert Hessian
#
# Warning message:
# In mle2(llgcnt, start = start, data = L, vecpar = TRUE) :
#   couldn't invert Hessian

soja$off <- 10
fivenum(with(soja, ngra/off))

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(ngra ~ offset(log(off)) + bloc + umid * K,
          data = soja, family = poisson)
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
          list(y = ngra, offset = off, X = model.matrix(m0)))

# Usa as estimativas do Poisson como valores iniciais.
start <- c(alpha = 0, coef(m0))
parnames(llgcnt) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llgcnt, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

# Estimaitvas dos parâmetros.
c0 <- cbind("PoissonGLM" = c(NA, coef(m0)),
            "PoissonML" = coef(m2),
            "GCount" = coef(m3))
c0

splom(c0[-(1:2), ]) + layer(panel.abline(a = 0, b = 1, lty = 2))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))
abline(v = 0, lty = 2)

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
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
                  data = cbind(off = 1, bloc = soja$bloc[1], pred))
i <- grep(x = colnames(X), pattern = "^bloc")
X[, i] <- X[, i] * 0 + 1/(length(i) + 1)

pred <- transform(pred,
                  K = as.numeric(as.character(K)),
                  umid = factor(umid))
pred <- list(pois = pred, quasi = pred, gcnt = pred)

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

# Preditos pela Gamma-Count.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "link")
pred$gcnt <- cbind(pred$gcnt, exp(aux[, c(2, 1, 3)]))

# Junta o resultado dos 3 modelos.
pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)
str(pred)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Gamma-Count")))

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

## ------------------------------------------------------------------------
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

start <- c(alpha = 0, coef(m0))
parnames(llgcnt) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llgcnt, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m3, m2)

c0 <- cbind("PoissonGLM" = c(NA, coef(m0)),
            "PoissonML" = coef(m2),
            "GCount" = coef(m3))
c0

splom(c0[-(1:2), ]) + layer(panel.abline(a = 0, b = 1, lty = 2))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
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

pred <- list(pois = pred, quasi = pred, gcnt = pred)

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

# Comparando as estimativas de média para contagem.
cbind(Pois = pred$pois$fit,
      Gcnt1 = c(exp(predict(m3, newdata = X))),
      GCnt2 = c(predict(m3, newdata = X, type = "response")))

# Preditos pela Gamma-Count.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "link")
pred$gcnt <- cbind(pred$gcnt, exp(aux[, c(2, 1, 3)]))

# Junta o resultado dos 3 modelos.
pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Gamma-Count")))

xyplot(ngra/nvag ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       xlim = extendrange(range(as.numeric(pred$K)), f = 0.2),
       key = key,
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de grãos por vagem",
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade")) +
    as.layer(
        xyplot(fit ~ K | umid, data = pred,
               layout = c(NA, 1), as.table = TRUE,
               pch = pred$modelo,
               ly = pred$lwr, uy = pred$upr, cty = "bars", length = 0,
               prepanel = prepanel.cbH,
               desloc = 0.15 * scale(as.integer(pred$modelo),
                                  scale = FALSE),
               panel = panel.cbH))

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------
# Número de capulhos em função do nível de desfolha artificial e fase
# fenológica do algodoeiro.

data(capdesfo, package = "MRDCr")
str(capdesfo)

# help(capdesfo, package = "MRDCr", help_type = "html")

p1 <- xyplot(ncap ~ des | est, data = capdesfo,
             col = 1, type = c("p", "smooth"), col.line = "gray50",
             layout = c(2, 3), as.table = TRUE,
             xlim = extendrange(capdesfo$des, f = 0.15),
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

# anova(m0, test = "Chisq")
anova(m1, test = "F")
summary(m1)

#-----------------------------------------------------------------------
# Modelo Poisson Generalizada.

L <- with(capdesfo,
          list(y = ncap, offset = 1, X = model.matrix(m0)))

start <- c(alpha = log(1), coef(m0))
parnames(llgcnt) <- names(start)

# Modelo Poisson também.
m2 <- mle2(llgcnt, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

c(logLik(m2), logLik(m0))

# Modelo Poisson Generalizado.
m3 <- mle2(llgcnt, start = start, data = L, vecpar = TRUE)
logLik(m3)

anova(m3, m2)

summary(m3)

c0 <- cbind("PoissonGLM" = c(NA, coef(m0)),
            "PoissonML" = coef(m2),
            "GCount" = coef(m3))
c0
splom(c0[-(1:2), ]) + layer(panel.abline(a = 0, b = 1, lty = 2))

# Perfil para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))

V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
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
pred <- list(pois = pred, quasi = pred, gcnt = pred)

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

# Comparando as estimativas de média para contagem.
cbind(Pois = pred$pois$fit,
      Gcnt1 = c(exp(predict(m3, newdata = X))),
      GCnt2 = c(predict(m3, newdata = X, type = "response")))[1:10, ]

# Preditos pela Gamma-Count.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "response")
pred$gcnt <- cbind(pred$gcnt, aux[, c(2, 1, 3)])

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, est, des, modelo)

key <- list(lines = list(lty = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Gamma-Count")))
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

## ---- fig.width=7, fig.height=3.5----------------------------------------
update(p1, type = "p", layout = c(NA, 1),
       key = key, spread = 0.07) +
    as.layer(p2, under = TRUE)

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------

data(nematoide, package = "MRDCr")
str(nematoide)

# help(nematoide, package = "MRDCr", help_type = "html")

# Número de nematóides por grama de raíz.
plot(nema ~ off, data = nematoide)

xyplot(nema/off ~ cult, data = nematoide,
       xlab = "Linhagens de feijão",
       ylab = "Número de nematoides por grama de raíz")

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
parnames(llgcnt) <- names(start)

# Modelo Poisson também.
m2 <- mle2(llgcnt, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

c(logLik(m2), logLik(m0))

# Poisson Generalizada.
m3 <- gcnt(formula(m0), data = nematoide)

# Diferença de deviance.
# 2 * diff(c(logLik(m0), logLik(m3)))
anova(m3, m2)

c0 <- cbind("PoissonGLM" = c(NA, coef(m0)),
            "PoissonML" = coef(m2),
            "GCount" = coef(m3))
c0
splom(c0[-(1:2), ]) + layer(panel.abline(a = 0, b = 1, lty = 2))

# Perfil de log-verossimilhança para o parâmetro de dispersão.
plot(profile(m3, which = "alpha"))

# Covariância.
V <- cov2cor(vcov(m3))
corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])
dev.off()

## ------------------------------------------------------------------------
# Tamanho das covariâncias com \alpha.
each(sum, mean, max)(abs(V[1, -1]))

#-----------------------------------------------------------------------
# Predição com intervalos de confiança.

pred <- unique(subset(nematoide, select = cult))
X <- model.matrix(~cult, data = pred)

pred <- list(pois = pred, quasi = pred, gcnt1 = pred, gcnt2 = pred)

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

# Preditos pela Gamma-Count.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "link")
pred$gcnt1 <- cbind(pred$gcnt1, exp(aux[, c(2, 1, 3)]))
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "response")
pred$gcnt2 <- cbind(pred$gcnt2, aux[, c(2, 1, 3)])

pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, cult, modelo)

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Gamma-Count 1", "Gamma-Count 2")))

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
               desloc = 0.15 * scale(as.integer(pred$modelo),
                                    scale = FALSE),
               panel = panel.cbH))

## ------------------------------------------------------------------------
#-----------------------------------------------------------------------
# Log-verossimilhança para o número de gols dos times em uma partida.

llgols <- function(theta, gh, ga, Xh, Xa){
    # theta: Vetor de parâmetros.
    # gh, ga: Gols do mandante e do visitante.
    # Xh, Xa: Matrizes de incidência das partidas.
    gamma <- 1:20  # Parâmetros de ataque.
    delta <- 21:40 # Parâmetros de defesa.
    omega <- 41    # Vantagem do mando de campo.
    alpha <- 42    # Dispersão da Gamma-Count. Se 0 então Poisson.
    #----------------------------------------
    # Preditor dos times mandantes e desafiante.
    eta.h <- (Xh %*% theta[gamma] - Xa %*% theta[delta]) + theta[omega]
    eta.a <- (Xa %*% theta[gamma] - Xh %*% theta[delta])
    # Parâmetro de dispersão.
    alpha <- exp(theta[alpha])
    #----------------------------------------
    # Quantidades auxiliares.
    alpha.gh <- alpha * gh
    alpha.eXb.h <- alpha * exp(eta.h)
    alpha.ga <- alpha * ga
    alpha.eXb.a <- alpha * exp(eta.a)
    offset <- 1
    #-------------------------------------------------------------------
    # Verossimilhanças.
    ll.h <- sum(log(pgamma(offset,
                           shape = alpha.gh,
                           rate = alpha.eXb.h) -
                    pgamma(offset,
                           shape = alpha.gh + alpha,
                           rate = alpha.eXb.h)))
    ll.a <- sum(log(pgamma(offset,
                           shape = alpha.ga,
                           rate = alpha.eXb.a) -
                    pgamma(offset,
                           shape = alpha.ga + alpha,
                           rate = alpha.eXb.a)))
    # Verossimilhança total.
    ll <- sum(ll.h + ll.a)
    return(-ll)
}

#-----------------------------------------------------------------------
# Análise dos jogos do Campeonado Brasileiro.

# Tomando as primeiras rodadas do campeonato.
db <- subset(cambras, rod <= 10)

# Quantos jogos cada time fez em casa e fora de casa.
addmargins(cbind(home = xtabs(~home, db),
                 away = xtabs(~away, db)), margin = 2)

subset(db, home == "Fluminense")

# Níveis na mesma ordem?
all(levels(db$home) == levels(db$away))

# Matrizes de delineamento.
Xh <- model.matrix(~ -1 + home, db) # h: home.
Xa <- model.matrix(~ -1 + away, db) # a: away.

# Verificação.
Xha <- Xh - Xa
db[1:5, c("home", "away")]
print.table(t(Xha[1:5, ]), zero.print = ".")

#-----------------------------------------------------------------------
# Ajuste do modelos.

# Valores iniciais para o modelo Poisson.
start <- c(rep(1, 40), 0.5, 0)
names(start) <- c(paste0("att", 1:20),
                  paste0("def", 1:20),
                  "hfa", "alpha")
parnames(llgols) <- names(start)

# Ajuste da Poisson, pois log(alpha) fixo em 0.
m0 <- mle2(minuslogl = llgols,
           start = start,
           data = list(gh = db$h, ga = db$a, Xh = Xh, Xa = Xa),
           fixed = list(alpha = 0),
           vecpar = TRUE)

# Valores iniciais para o modelo Gamma-Count.
start <- coef(m0)
parnames(llgols) <- names(start)

# Ajuste da Gamma-Count, alpha será estimado.
m2 <- mle2(minuslogl = llgols,
           start = start,
           data = list(gh = db$h, ga = db$a, Xh = Xh, Xa = Xa),
           vecpar = TRUE)

#-----------------------------------------------------------------------
# Comparando resultados.

# Estimativas dos parâmetros.
c0 <- data.frame(Pois = coef(m0),
                 GCnt = coef(m2))

xyplot(GCnt ~ Pois, data = c0, aspect = "iso",
       groups = gsub(x = rownames(c0), "\\d", ""),
       auto.key = TRUE, grid = TRUE) +
    layer(panel.abline(a = 0, b = 1))

# Teste para o parâmetro de dispersão.
anova(m0, m2)

# Função de risco.
al <- exp(coef(m2)[42])
curve(dgamma(x, al, 1)/(1 - pgamma(x, al, 1)),
      from = 0, to = 2,
      xlab = "t",
      ylab = expression(h(t) == f(t)/(1 - F(t))))

#-----------------------------------------------------------------------
# Estimativas intervalares.

ci <- rbind(cbind(1, coef(m0)[-42], confint(m0, method = "quad")),
            cbind(2, coef(m2), confint(m2, method = "quad")))
ci <- as.data.frame(ci)
names(ci) <- c("model", "est", "lwr", "upr")
ci$model <- factor(ci$model, labels = c("Pois", "GCnt"))
ci$par <- factor(sub(pattern = "\\d+",
                     replacement = "",
                     x = rownames(ci)))
ci$team <- factor(levels(db$home)[
    as.numeric(sub(pattern = "\\D+",
                   replacement = "",
                   x = rownames(ci)))])

sb <- subset(ci, par == "att" & model == "GCnt")
ci$team <- factor(ci$team, levels = sb$team[order(sb$est)])

segplot(team ~ lwr + upr | model,
        centers = est, data = ci,
        draw = FALSE, groups = par, gap = 0.2,
        pch = as.integer(ci$par),
        key = list(title = "Parâmetro", cex.title = 1.1,
                   type = "o", divide = 1,
                   text = list(c("Ataque", "Defesa")),
                   lines = list(pch = 2:3)),
        ylab = "Times (ordenados pelo ataque)",
        xlab = "Estimativa do parâmetro",
        panel = panel.groups.segplot)

ci <- arrange(ci, par, team, model)
segplot(team ~ lwr + upr | par,
        centers = est, data = subset(ci, par %in% c("att", "def")),
        draw = FALSE, groups = model, gap = 0.2,
        pch = as.integer(ci$model),
        key = list(title = "Modelo", cex.title = 1.1,
                   type = "o", divide = 1,
                   text = list(c("Poisson", "Gamma-Count")),
                   lines = list(pch = 2:1)),
        ylab = "Times (ordenados pelo ataque)",
        xlab = "Estimativa do parâmetro",
        panel = panel.groups.segplot)


#-----------------------------------------------------------------------
# Gols observados e preditos dos times mandantes e desafiantes nestas
# rodadas.

gg <-
rbind(
    cbind(x = 1,
          y = db$h,
          Pois = c(exp(cbind(Xh, -Xa) %*%
                       coef(m0)[1:40] + coef(m0)["hfa"])),
          GCnt1 = c(exp(cbind(Xh, -Xa) %*%
                        coef(m2)[1:40] + coef(m2)["hfa"])),
          GCnt2 = calc_mean_gcnt(lambda = exp(cbind(Xh, -Xa) %*%
                                              coef(m2)[1:40] +
                                              coef(m2)["hfa"]),
                                 alpha = exp(coef(m2)["alpha"]))),
    cbind(x = 2,
          y = db$a,
          Pois = c(exp(cbind(Xa, -Xh) %*% coef(m0)[1:40])),
          GCnt1 = c(exp(cbind(Xa, -Xh) %*% coef(m2)[1:40])),
          GCnt2 = calc_mean_gcnt(lambda = exp(cbind(Xa, -Xh) %*%
                                              coef(m2)[1:40]),
                                 alpha = exp(coef(m2)["alpha"]))))

# Comparação de observado com predito.
splom(gg[, -1], groups = gg[, 1]) +
    layer(panel.abline(a = 0, b = 1))

#-----------------------------------------------------------------------
# Fazendo previsão dos resultados da rodada a seguir.

levels(db$home)

# # Na rodada 11 deu Cruzeiro 2x2 Grêmio.
# i <- which(levels(db$home) %in% c("Cruzeiro", "Grêmio"))
# levels(db$home)[i]

# Na rodada 11 dei Ceará 0x0 Palmeiras.
i <- which(levels(db$home) %in% c("Ceará", "Palmeiras"))
levels(db$home)[i]

# Dois times jogando em território neutro.
# HomeAttack - AwayDefense
# AwayAttack - HomeDefense
alp <- exp(coef(m2)[42])
beta <- exp(coef(m2)[i] - rev(coef(m2)[20 + i]))
names(beta) <- levels(db$home)[i]
beta

# Tempo médio entre Gols.
exp(-beta)

# Probabilidade dos placares de 0x0, 0x1, ..., 6x6.
y <- 0:6
dnn <- lapply(substr(levels(db$home)[i], 0, 3), FUN = paste, y)
plapois <- outer(dgcnt(y = y, lambda = beta[1], alpha = 1),
                 dgcnt(y = y, lambda = beta[2], alpha = 1),
                 FUN = "*")
plagcnt <- outer(dgcnt(y = y, lambda = beta[1], alpha = alp),
                 dgcnt(y = y, lambda = beta[2], alpha = alp),
                 FUN = "*")
dimnames(plapois) <- dnn
dimnames(plagcnt) <- dnn

print.table(round(plapois, 2), zero.print = ".")
print.table(round(plagcnt, 2), zero.print = ".")

# Olhando só para os empates.
round(cbind(Pois = diag(plapois),
            GCnt = diag(plagcnt)), 3)

# Draw - Win - Lose.
dwl <- rbind(Pois = c(sum(diag(plapois)),
                      sum(plapois[lower.tri(plapois)]),
                      sum(plapois[upper.tri(plapois)])),
             GCnt = c(sum(diag(plagcnt)),
                      sum(plagcnt[lower.tri(plagcnt)]),
                      sum(plagcnt[upper.tri(plagcnt)])))
colnames(dwl) <- sprintf(paste(levels(db$home)[i], collapse = " %s "),
                         c("=", ">", "<"))
t(dwl)

## ---- echo=FALSE, results="hold"-----------------------------------------
cat(format(Sys.time(),
           format = "Atualizado em %d de %B de %Y.\n\n"))
sessionInfo()

