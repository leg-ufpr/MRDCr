## ----setup, include=FALSE-----------------------------------------
source("_setup.R")

## -----------------------------------------------------------------

library(MRDCr)


## -----------------------------------------------------------------

llcmp


## -----------------------------------------------------------------

cmp


## -----------------------------------------------------------------

set.seed(2016)
x <- rep(1:3, 2)
t <- rnorm(6, 5)
y <- rpois(6, lambda = x*t)
(da <- data.frame(x, t, y))

## Definindo o prditor do modelo
formula <- y ~ x + I(x^2) + offset(log(t))

##-------------------------------------------
## O framework

## Constrói as matrizes para ajuste do modelo
frame <- model.frame(formula, data = da)
(X <- model.matrix(formula, data = da))
(y <- model.response(frame))
(o <- model.offset(frame))

## Utiliza como valores iniciais as estimativas dos parametros de um
## modelo GLM-Poisson
m0 <- glm.fit(x = X, y = y, offset = o, family = poisson())
start <- c(phi = 0, m0$coefficients)

## Otimiza a função de log-verossimilhança via bbmle
library(bbmle)
parnames(llcmp) <- names(start)
mle2(llcmp, start = start,
     data = list(y = y, X = X, offset = o, sumto = 50),
     vecpar = TRUE)


## -----------------------------------------------------------------

data(capdesfo)
str(capdesfo)
## help(capdesfo)


## -----------------------------------------------------------------

## Experimento balanceado
xtabs(~est + des, data = capdesfo)

library(lattice)
library(latticeExtra)

(xy <- xyplot(ncap ~ des | est,
             data = capdesfo,
             xlab = "Nível de desfolha artificial",
             ylab = "Número de capulhos produzidos",
             type = c("p", "g", "smooth"),
             panel = panel.beeswarm,
             r = 0.05))

## Avaliando preliminarmente suposição de equidispersão
(mv <- aggregate(ncap ~ est + des, data = capdesfo,
                 FUN = function(x) c(mean = mean(x), var = var(x))))

xlim <- ylim <- extendrange(c(mv$ncap), f = 0.05)
xyplot(ncap[, "var"] ~ ncap[, "mean"],
       data = mv,
       xlim = xlim,
       ylim = ylim,
       ylab = "Variância Amostral",
       xlab = "Média Amostral",
       panel = function(x, y) {
           panel.xyplot(x, y, type = c("p", "r"), grid = TRUE)
           panel.abline(a = 0, b = 1, lty = 2)
       })


## -----------------------------------------------------------------

## Preditores considerados
f1 <- ncap ~ 1
f2 <- ncap ~ des + I(des^2)
f3 <- ncap ~ est:des + I(des^2)
f4 <- ncap ~ est:(des + I(des^2))

## Ajustando os modelos Poisson
m1P <- glm(f1, data = capdesfo, family = poisson)
m2P <- glm(f2, data = capdesfo, family = poisson)
m3P <- glm(f3, data = capdesfo, family = poisson)
m4P <- glm(f4, data = capdesfo, family = poisson)

## Ajustando os modelos COM-Poisson
m1C <- cmp(f1, data = capdesfo)
m2C <- cmp(f2, data = capdesfo)
m3C <- cmp(f3, data = capdesfo)
m4C <- cmp(f4, data = capdesfo)


## -----------------------------------------------------------------

## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(list(m1P, m2P, m3P, m4P), logLik),
      "COM-Poisson" = sapply(list(m1C, m2C, m3C, m4C), logLik))

## Teste de razão de verossimilhanças
anova(m1P, m2P, m3P, m4P, test = "Chisq")
anova(m1C, m2C, m3C, m4C)


## -----------------------------------------------------------------

## Estimativas dos parâmetros
summary(m4P)
summary(m4C)


## -----------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária
convergencez(m4C)


## -----------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada
par(mfrow = c(2, 2))
plot(m4P)


## -----------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz ve variâncias e covariâncias
Vcov <- vcov(m4C)
Corr <- cov2cor(Vcov)

library(corrplot)
corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## -----------------------------------------------------------------

## Predição pontual
pred <- with(capdesfo,
             expand.grid(
                 est = levels(est),
                 des = seq(min(des), max(des), l = 20)
             ))

##-------------------------------------------
## Considerando a Poisson
mediasP <- exp(predict(m4P, newdata = pred))
aux <- data.frame(modelo = "Poisson", fit = mediasP)
predP <- cbind(pred, aux)

##-------------------------------------------
## Considerando a COM-Poisson
f4; f4[[2]] <- NULL; f4
X <- model.matrix(f4, data = pred)

## Obtendo os parâmetros da distribuição (lambdas e phi)
betas <- coef(m4C)[-1]
phi <- coef(m4C)[1]
loglambdas <- X %*% betas

## Aplicando a "inversa da função de ligação", ou seja, obtendo as
## contagens médias
mediasC <- sapply(loglambdas, FUN = function(p) {
    y <- 0:50; py <- dcmp(y, p, phi, sumto = 100)
    sum(y*py)
})
aux <- data.frame(modelo = "COM-Poisson", fit = mediasC)
predC <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos pelos dois modelos
da <- rbind(predP, predC)

update(xy, type = c("p", "g")) +
    as.layer(xyplot(fit ~ des | est,
                    groups = modelo,
                    data = da, type = "l"))


## -----------------------------------------------------------------

## Predição intervalar
qn <- qnorm(0.975) * c(lwr = -1, upr = 1)

##-------------------------------------------
## Considerando a Poisson
aux <- predict(m4P, newdata = pred, se.fit = TRUE)
aux <- with(aux, exp(fit + outer(se.fit, qn, FUN = "*")))
predP <- cbind(predP, aux)

##-------------------------------------------
## Considerando a COM-Poisson

## Obtendo os erros padrão das estimativas
##   Obs.: Deve-se usar a matriz de variâncias e covariâncias
##   condicional, pois os parâmetros de locação (betas) e dispersão
##   (phi) não são ortogonais.
Vc <- Vcov[-1, -1] - Vcov[-1, 1] %*% solve(Vcov[1, 1]) %*% Vcov[1, -1]
U <- chol(Vc)
se <- sqrt(apply(X %*% t(U), MARGIN = 1, FUN = function(x) {
    sum(x^2)
}))

aux <- c(loglambdas) + outer(se, qn, FUN = "*")
aux <- apply(aux, MARGIN = 2,
             FUN = function(col) {
                 sapply(col, FUN = function(p) {
                     y <- 0:50; py <- dcmp(y, p, phi, sumto = 100)
                     sum(y*py)
                 })
             })
predC <- cbind(predC, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC)

update(xy, type = c("p", "g")) +
    as.layer(xyplot(fit ~ des | est,
                    groups = modelo,
                    data = da,
                    type = "l",
                    ly = da$lwr,
                    uy = da$upr,
                    cty = "bands",
                    alpha = 0.3,
                    prepanel = prepanel.cbH,
                    panel.groups = panel.cbH,
                    panel = panel.superpose))


## -----------------------------------------------------------------

data(capmosca)
str(capmosca)
## help(capmosca)


## -----------------------------------------------------------------

capmosca <- aggregate(ncap ~ vaso + dexp, data = capmosca, FUN = sum)
str(capmosca)


## -----------------------------------------------------------------

## Experimento balanceado
xtabs(~dexp, data = capmosca)

(xy <- xyplot(ncap ~ dexp,
              data = capmosca,
              xlab = "Dias de infestação",
              ylab = "Número de capulhos produzidos",
              type = c("p", "g", "smooth"),
              panel = panel.beeswarm,
              r = 0.05))

## Avaliando preliminarmente suposição de equidispersão
(mv <- aggregate(ncap ~ dexp, data = capmosca,
                 FUN = function(x) c(mean = mean(x), var = var(x))))


## -----------------------------------------------------------------

## Preditores considerados
f1 <- ncap ~ 1
f2 <- ncap ~ dexp
f3 <- ncap ~ dexp + I(dexp^2)

## Ajustando os modelos Poisson
m1P <- glm(f1, data = capmosca, family = poisson)
m2P <- glm(f2, data = capmosca, family = poisson)
m3P <- glm(f3, data = capmosca, family = poisson)

## Ajustando os modelos COM-Poisson
m1C <- cmp(f1, data = capmosca)
m2C <- cmp(f2, data = capmosca)
m3C <- cmp(f3, data = capmosca)


## -----------------------------------------------------------------

## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(list(m1P, m2P, m3P), logLik),
      "COM-Poisson" = sapply(list(m1C, m2C, m3C), logLik))

## Teste de razão de verossimilhanças
anova(m1P, m2P, m3P, test = "Chisq")
anova(m1C, m2C, m3C)


## -----------------------------------------------------------------

## Estimativas dos parâmetros
summary(m3P)
summary(m3C)


## -----------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária
convergencez(m3C)


## -----------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada
par(mfrow = c(2, 2))
plot(m3P)


## -----------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz ve variâncias e covariâncias
Vcov <- vcov(m3C)
Corr <- cov2cor(Vcov)

library(corrplot)
corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## -----------------------------------------------------------------

## Predição pontual/intervalar
pred <- with(capmosca,
             expand.grid(
                 dexp = seq(min(dexp), max(dexp), l = 50)
             ))
qn <- qnorm(0.975) * c(fit = 0, lwr = -1, upr = 1)

##-------------------------------------------
## Considerando a Poisson
aux <- predict(m3P, newdata = pred, se.fit = TRUE)
aux <- with(aux, exp(fit + outer(se.fit, qn, FUN = "*")))
aux <- data.frame(modelo = "Poisson", aux)
predP <- cbind(pred, aux)

##-------------------------------------------
## Considerando a COM-Poisson
f3; f3[[2]] <- NULL; f3
X <- model.matrix(f3, data = pred)

## Obtendo os parâmetros da distribuição (lambdas e phi)
betas <- coef(m3C)[-1]
phi <- coef(m3C)[1]
loglambdas <- X %*% betas

## Obtendo os erros padrão das estimativas
##   Obs.: Deve-se usar a matriz de variâncias e covariâncias
##   condicional, pois os parâmetros de locação (betas) e dispersão
##   (phi) não são ortogonais.
Vc <- Vcov[-1, -1] - Vcov[-1, 1] %*% solve(Vcov[1, 1]) %*% Vcov[1, -1]
U <- chol(Vc)
se <- sqrt(apply(X %*% t(U), MARGIN = 1, FUN = function(x) {
    sum(x^2)
}))

aux <- c(loglambdas) + outer(se, qn, FUN = "*")
aux <- apply(aux, MARGIN = 2,
             FUN = function(col) {
                 sapply(col, FUN = function(p) {
                     y <- 0:30; py <- dcmp(y, p, phi, sumto = 50)
                     sum(y*py)
                 })
             })
aux <- data.frame(modelo = "COM-Poisson", aux)
predC <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC)

update(xy, type = c("p", "g")) +
    as.layer(xyplot(fit ~ dexp,
                    groups = modelo,
                    data = da,
                    type = "l",
                    ly = da$lwr,
                    uy = da$upr,
                    cty = "bands",
                    alpha = 0.3,
                    prepanel = prepanel.cbH,
                    panel.groups = panel.cbH,
                    panel = panel.superpose))


