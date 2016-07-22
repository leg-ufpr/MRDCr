## ----setup, include=FALSE------------------------------------------------
source("_setup.R")

## ------------------------------------------------------------------------

library(MRDCr)


## ------------------------------------------------------------------------
llcmp

## ------------------------------------------------------------------------
cmp

## ------------------------------------------------------------------------
set.seed(2016)
x <- rep(1:3, 3)
y <- rpois(9, lambda = x)
(da <- data.frame(x, y))

## Definindo o prditor do modelo
formula <- y ~ x + I(x^2)

##-------------------------------------------
## O framework

## Constrói as matrizes para ajuste do modelo
frame <- model.frame(formula, data = da)
(X <- model.matrix(formula, data = da))
(y <- model.response(frame))

## Utiliza como valores iniciais as estimativas dos parametros de um
## modelo GLM-Poisson
m0 <- glm.fit(x = X, y = y, family = poisson())
(start <- c(phi = 0, m0$coefficients))

## Otimiza a função de log-verossimilhança via bbmle
library(bbmle)
parnames(llcmp) <- names(start)
mle2(llcmp, start = start,
     data = list(y = y, X = X, sumto = 50),
     vecpar = TRUE)


## ------------------------------------------------------------------------

data(capmosca)
str(capmosca)
## help(capmosca)


## ------------------------------------------------------------------------

capmosca <- aggregate(ncap ~ vaso + dexp, data = capmosca, FUN = sum)
str(capmosca)


## ------------------------------------------------------------------------

## Experimento balanceado
xtabs(~dexp, data = capmosca)

library(lattice)
library(latticeExtra)

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


## ------------------------------------------------------------------------

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


## ------------------------------------------------------------------------

## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(list(m1P, m2P, m3P), logLik),
      "COM-Poisson" = sapply(list(m1C, m2C, m3C), logLik))

## Teste de razão de verossimilhanças
anova(m1P, m2P, m3P, test = "Chisq")
anova(m1C, m2C, m3C)


## ------------------------------------------------------------------------

## Estimativas dos parâmetros
summary(m3P)
summary(m3C)


## ------------------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária
convergencez(m3C)

## Reajustando o modelo
logLik(m3C <- cmp(f3, data = capmosca, sumto = 30))


## ------------------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada
par(mfrow = c(2, 2))
plot(m3P)


## ---- cache = TRUE-------------------------------------------------------

##-------------------------------------------
## Testando a nulidade do parâmetro phi

## Usando o ajuste Poisson
trv <- 2 * (logLik(m3C) - logLik(m3P))
attributes(trv) <- NULL
round(c(trv, pchisq(trv, 1, lower = FALSE)), digits = 5)

## Reajustando o COM-Poisson para phi = 0 (ou equivalente nu = 1)
m3Cfixed <- cmp(f3, data = capmosca, fixed = list("phi" = 0))
anova(m3C, m3Cfixed)

## Via perfil de log-verossimilhança
perf <- profile(m3C, which = 1)
confint(perf)
plot(perf)


## ------------------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz ve variâncias e covariâncias
Vcov <- vcov(m3C)
Corr <- cov2cor(Vcov)

library(corrplot)
corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## ------------------------------------------------------------------------

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
f3; f3[-2]
X <- model.matrix(f3[-2], data = pred)

aux <- predict(m3C, newdata = X, type = "response",
               interval = "confidence")
aux <- data.frame(modelo = "COM-Poisson", aux)
predC <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC)

## Legenda
cols <- trellis.par.get("superpose.line")$col[1:2]
key <- list(
    lines = list(lty = 1, col = cols),
    rect = list(col = cols, alpha = 0.1, lty = 3, border = NA),
    text = list(c("COM-Poisson", "Poisson")))

## Gráfico dos valores preditos e IC de 95% para média
update(xy, type = c("p", "g"), key = key, alpha = 0.7) +
    as.layer(xyplot(fit ~ dexp,
                    groups = modelo,
                    data = da,
                    type = "l",
                    ly = da$lwr,
                    uy = da$upr,
                    cty = "bands",
                    alpha = 0.5,
                    prepanel = prepanel.cbH,
                    panel.groups = panel.cbH,
                    panel = panel.superpose))


## ------------------------------------------------------------------------

data(soja)
str(soja)
## help(soja)


## ------------------------------------------------------------------------

## Observação 74 foi diagnosticada como outlier
soja <- soja[-74, ]
soja <- transform(soja, K = factor(K))


## ---- fig.height = 12----------------------------------------------------

## Experimento (des)balanceado
xtabs(~K + umid, data = soja)

key <- list(
    type = "b", divide = 1,
    ## points = list(pch = 1, col = cols),
    lines = list(pch = 1, lty = 1, col = cols),
    text = list(c("Nº de grãos por parcela", "Nº de vagens viáveis")))

xyplot(ngra + nvag ~ K | umid,
       data = soja,
       xlab = "Nível de adubação potássica",
       ylab = "Contagem",
       type = c("p", "g", "smooth"),
       key = key,
       layout = c(NA, 1),
       strip = strip.custom(
           strip.names = TRUE, var.name = "Umidade"))


## ------------------------------------------------------------------------

## Avaliando preliminarmente suposição de equidispersão
(mv <- aggregate(cbind(ngra, nvag, ng2v = ngra/nvag) ~ K + umid,
                 data = soja, FUN = function(x)
                     c(mean = mean(x), var = var(x))))

##-------------------------------------------
## Para o número de grãos
xlim <- ylim <- extendrange(c(mv$ngra), f = 0.05)
mvp1 <- xyplot(ngra[, "var"] ~ ngra[, "mean"],
               data = mv,
               xlim = xlim, ylim = ylim,
               ylab = "Variância Amostral",
               xlab = "Média Amostral",
               main = "Número de grãos por parcela",
               panel = function(x, y) {
                   panel.xyplot(x, y, type = c("p", "r"), grid = TRUE)
                   panel.abline(a = 0, b = 1, lty = 2)
               })

##-------------------------------------------
## Para o número de vagens
xlim <- ylim <- extendrange(c(mv$nvag), f = 0.05)
mvp2 <- xyplot(nvag[, "var"] ~ nvag[, "mean"],
               data = mv,
               xlim = xlim, ylim = ylim,
               ylab = "Variância Amostral",
               xlab = "Média Amostral",
               main = "Número de vagens viáveis",
               panel = function(x, y) {
                   panel.xyplot(x, y, type = c("p", "r"), grid = TRUE)
                   panel.abline(a = 0, b = 1, lty = 2)
               })

print(mvp1, split = c(1, 1, 2, 1), more = TRUE)
print(mvp2, split = c(2, 1, 2, 1), more = FALSE)


## ----ajuste2, cache = TRUE-----------------------------------------------

##-------------------------------------------
## Para o número de vagens viáveis por parcela

## Preditores considerados
f1.nv <- nvag ~ bloc + umid + K
f2.nv <- nvag ~ bloc + umid * K

## Ajustando os modelos Poisson
m1P.nv <- glm(f1.nv, data = soja, family = poisson)
m2P.nv <- glm(f2.nv, data = soja, family = poisson)

## Ajustando os modelos COM-Poisson
m1C.nv <- cmp(f1.nv, data = soja, sumto = 300)
m2C.nv <- cmp(f2.nv, data = soja, sumto = 300)

##-------------------------------------------
## Para o número grão produzidos por parcela

## Preditores considerados
f1.ng <- ngra ~ bloc + umid + K
f2.ng <- ngra ~ bloc + umid * K

## Ajustando os modelos Poisson
m1P.ng <- glm(f1.ng, data = soja, family = poisson)
m2P.ng <- glm(f2.ng, data = soja, family = poisson)

## Ajustando os modelos COM-Poisson
m1C.ng <- cmp(f1.ng, data = soja, sumto = 700)
m2C.ng <- cmp(f2.ng, data = soja, sumto = 700)


## ------------------------------------------------------------------------

##-------------------------------------------
## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(
          list("nvagem adtv" = m1P.nv, "nvagem mult" = m2P.nv,
               "ngraos adtv" = m1P.ng, "ngraos mult" = m2P.ng),
          logLik),
      "COM-Poisson" = sapply(
          list(m1C.nv, m2C.nv, m1C.ng, m2C.ng),
          logLik))

##-------------------------------------------
## Teste de razão de verossimilhanças

## Dos modelos para o número de vagens viáveis
anova(m1P.nv, m2P.nv, test = "Chisq")
anova(m1C.nv, m2C.nv)

## Dos modelos para o número de grão por parcela
anova(m1P.ng, m2P.ng, test = "Chisq")
anova(m1C.ng, m2C.ng)


## ------------------------------------------------------------------------
##-------------------------------------------
## Estimativas dos parâmetros do modelo proposto para

## o número de vagens viáveis
summary(m2P.nv)
summary(m2C.nv)

## o número de grão por parcela
summary(m2P.ng)
summary(m2C.ng)


## ------------------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária

## No modelo para o número de vagens viáveis
convergencez(m2C.nv)

## No modelo para o número de grão por parcela
convergencez(m2C.ng)


## ------------------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada

## Avaliação do modelo para o número de vagens viáveis
par(mfrow = c(2, 2))
plot(m2P.nv)

## Avaliação do modelo para o número de grão por parcela
par(mfrow = c(2, 2))
plot(m2P.ng)


## ---- cache = TRUE-------------------------------------------------------

## Testando a nulidade do parâmetro phi

##-------------------------------------------
## No modelo para o número de vagens viáveis

## Usando o ajuste Poisson
trv <- 2 * (logLik(m2C.nv) - logLik(m2P.nv))
attributes(trv) <- NULL
round(c(trv, pchisq(trv, 1, lower = FALSE)), digits = 5)

## Reajustando o COM-Poisson para phi = 0 (ou equivalente nu = 1)
m2Cfixed.nv <- cmp(f2.nv, data = soja, fixed = list("phi" = 0),
                   sumto = 300)
anova(m2C.nv, m2Cfixed.nv)

##-------------------------------------------
## No modelo para o número de grão por parcela

## Usando o ajuste Poisson
trv <- 2 * (logLik(m2C.ng) - logLik(m2P.ng))
attributes(trv) <- NULL
round(c(trv, pchisq(trv, 1, lower = FALSE)), digits = 5)

## Reajustando o COM-Poisson para phi = 0 (ou equivalente nu = 1)
m2Cfixed.ng <- cmp(f2.ng, data = soja, fixed = list("phi" = 0),
                   sumto = 700)
anova(m2C.ng, m2Cfixed.ng)


## ----perf2, echo = FALSE, warnings = FALSE-------------------------------

##-------------------------------------------
## Via perfis de log-verossimilhança
perf.nv <- profile(m2C.nv, which = "phi")
perf.ng <- profile(m2C.ng, which = "phi")

confint(perf.nv)
confint(perf.ng)

par(mfrow = c(1, 2))
plot(perf.nv)
plot(perf.ng)


## ------------------------------------------------------------------------

## Verificando a matriz de variâncias e covariâncias

##-------------------------------------------
## No modelo para o número de vagens viáveis
Vcov.nv <- vcov(m2C.nv)
Corr <- cov2cor(Vcov.nv)

corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])

##-------------------------------------------
## No modelo para o número de grão por parcela
Vcov.ng <- vcov(m2C.ng)
Corr <- cov2cor(Vcov.ng)

corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## ---- fig.height = 18----------------------------------------------------

## Predição pontual/intervalar
pred <- with(soja,
             expand.grid(
                 bloc = factor(levels(bloc)[1], levels = levels(bloc)),
                 umid = levels(umid),
                 K = levels(K)
                 ## nvag = 1
             ))
qn <- qnorm(0.975) * c(fit = 0, lwr = -1, upr = 1)

## Definindos as matrizes de delineamento

## Do modelo com interação
f2.ng; f2.ng[-2]
X2 <- model.matrix(f2.ng[-2], data = pred)

## Como não temos interesse na interpretação dos efeitos de blocos
## tomaremos a média desses efeitos para predição
bl2 <- attr(X2, "assign") == 1
X2[, bl2] <- X2[, bl2] * 0 + 1/(sum(bl2) + 1)
head(X2)

## Do modelo aditivo, apenas retiramos os termos referentes ao efeito de
## interação
X1 <- X2[, attr(X2, "assign") != 4]
head(X1)

library(multcomp)

##-------------------------------------------
## Considerando a Poisson

## No modelo para o número de vagens
aux <- exp(confint(glht(m2P.nv, linfct = X2),
               calpha = univariate_calpha())$confint)
colnames(aux) <- c("fit", "lwr", "upr")
aux <- data.frame(modelo = "Poisson", aux)
predP.nv <- cbind(pred, aux)

## No modelo para o número de grãos por parcela
aux <- exp(confint(glht(m2P.ng, linfct = X2),
               calpha = univariate_calpha())$confint)
colnames(aux) <- c("fit", "lwr", "upr")
aux <- data.frame(modelo = "Poisson", aux)
predP.ng <- cbind(pred, aux)

##----------------------------------------------------------------------
## Considerando a COM-Poisson

## No modelo para o número de vagens
aux <- predict(m2C.nv, newdata = X2, type = "response",
               interval = "confidence")
aux <- data.frame(modelo = "COM-Poisson", aux)
predC.nv <- cbind(pred, aux)

## No modelo para o número de grãos por parcela
aux <- predict(m2C.ng, newdata = X2, type = "response",
               interval = "confidence")
aux <- data.frame(modelo = "COM-Poisson", aux)
predC.ng <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da.nv <- rbind(predP.nv, predC.nv)
da.nv <- da.nv[with(da.nv, order(umid, K, modelo)), ]

da.ng <- rbind(predP.ng, predC.ng)
da.ng <- da.ng[with(da.ng, order(umid, K, modelo)), ]

source(paste0("https://gitlab.c3sl.ufpr.br/leg/legTools/raw/",
              "issue%2315/R/panel.segplot.by.R"))

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(da.nv$modelo) + 3, lty = 1),
            text = list(c("Poisson", "COM-Poisson")))

xy1 <- xyplot(nvag ~ K | umid, data = soja,
       xlab = "Nível de adubação potássica",
       ylab = "Contagem",
       type = c("p", "g"), alpha = 0.7,
       key = key,
       layout = c(NA, 1),
       as.table = TRUE,
       strip = strip.custom(
           strip.names = TRUE, var.name = "Umidade")) +
    as.layer(
        segplot(
            K ~ lwr + upr | umid,
            centers = fit, groups = modelo, data = da.nv,
            grid = TRUE, horizontal = FALSE, draw = FALSE,
            lwd = 2, pch = 1:nlevels(da$modelo) + 3,
            panel = panel.segplot.by, f = 0.1, as.table = TRUE)
    )

xy2 <- xyplot(ngra ~ K | umid, data = soja,
       xlab = "Nível de adubação potássica",
       ylab = "Contagem",
       type = c("p", "g"), alpha = 0.7,
       ## key = key,
       layout = c(NA, 1),
       as.table = TRUE,
       strip = strip.custom(
           strip.names = TRUE, var.name = "Umidade")) +
    as.layer(
        segplot(
            K ~ lwr + upr | umid,
            centers = fit, groups = modelo, data = da.ng,
            grid = TRUE, horizontal = FALSE, draw = FALSE,
            lwd = 2, pch = 1:nlevels(da$modelo) + 3,
            panel = panel.segplot.by, f = 0.1, as.table = TRUE)
    )

## x11(width = 10, height = 50)
print(xy1, split = c(1, 1, 1, 2), more = TRUE)
print(xy2, split = c(1, 2, 1, 2), more = FALSE)


## ------------------------------------------------------------------------

data(capdesfo)
str(capdesfo)
## help(capdesfo)


## ------------------------------------------------------------------------

## Experimento balanceado
xtabs(~est + des, data = capdesfo)

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
       xlim = xlim, ylim = ylim,
       ylab = "Variância Amostral",
       xlab = "Média Amostral",
       panel = function(x, y) {
           panel.xyplot(x, y, type = c("p", "r"), grid = TRUE)
           panel.abline(a = 0, b = 1, lty = 2)
       })


## ------------------------------------------------------------------------

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


## ------------------------------------------------------------------------

## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(list(m1P, m2P, m3P, m4P), logLik),
      "COM-Poisson" = sapply(list(m1C, m2C, m3C, m4C), logLik))

## Teste de razão de verossimilhanças
anova(m1P, m2P, m3P, m4P, test = "Chisq")
anova(m1C, m2C, m3C, m4C)


## ------------------------------------------------------------------------

## Estimativas dos parâmetros
summary(m4P)
summary(m4C)


## ------------------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária
convergencez(m4C)

## Reajustando o modelo
logLik(m4C <- cmp(f4, data = capdesfo, sumto = 30))


## ------------------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada
par(mfrow = c(2, 2))
plot(m4P)


## ---- cache = TRUE-------------------------------------------------------

##-------------------------------------------
## Testando a nulidade do parâmetro phi

## Usando o ajuste Poisson
trv <- 2 * (logLik(m4C) - logLik(m4P))
attributes(trv) <- NULL
round(c(trv, pchisq(trv, 1, lower = FALSE)), digits = 5)

## Reajustando o COM-Poisson para phi = 0 (ou equivalente nu = 1)
m4Cfixed <- cmp(f4, data = capdesfo, fixed = list("phi" = 0))
anova(m4C, m4Cfixed)


## ----perf3, cache = TRUE-------------------------------------------------

## Via perfil de log-verossimilhança
perf <- profile(m4C, which = "phi")
confint(perf)
plot(perf)


## ------------------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz ve variâncias e covariâncias
Vcov <- vcov(m4C)
Corr <- cov2cor(Vcov)

library(corrplot)
corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## ------------------------------------------------------------------------

## Predição pontual/intervalar
pred <- with(capdesfo,
             expand.grid(
                 est = levels(est),
                 des = seq(min(des), max(des), l = 20)
             ))
qn <- qnorm(0.975) * c(fit = 0, lwr = -1, upr = 1)

##-------------------------------------------
## Considerando a Poisson
aux <- predict(m4P, newdata = pred, se.fit = TRUE)
aux <- with(aux, exp(fit + outer(se.fit, qn, FUN = "*")))
aux <- data.frame(modelo = "Poisson", aux)
predP <- cbind(pred, aux)

##-------------------------------------------
## Considerando a COM-Poisson
f4; f4[-2]
X <- model.matrix(f4[-2], data = pred)

aux <- predict(m4C, newdata = X, type = "response",
               interval = "confidence")
aux <- data.frame(modelo = "COM-Poisson", aux)
predC <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC)

## Legenda
cols <- trellis.par.get("superpose.line")$col[1:2]
key <- list(
    lines = list(lty = 1, col = cols),
    rect = list(col = cols, alpha = 0.1, lty = 3, border = NA),
    text = list(c("COM-Poisson", "Poisson")))

## Gráfico dos valores preditos e IC de 95% para média
update(xy, type = c("p", "g"), key = key, alpha = 0.7) +
    as.layer(xyplot(fit ~ des | est,
                    groups = modelo,
                    data = da,
                    type = "l",
                    ly = da$lwr,
                    uy = da$upr,
                    cty = "bands",
                    alpha = 0.5,
                    prepanel = prepanel.cbH,
                    panel.groups = panel.cbH,
                    panel = panel.superpose))


## ------------------------------------------------------------------------

## Reparametriza para a média (aproximada)
llcmp3 <- function (params, y, X, offset = NULL, sumto = 50) {
    betas <- params[-1]
    phi <- params[1]
    nu <- exp(phi)
    if (is.null(offset))
        offset <- 0
    ##-------------------------------------------
    ## Reparametrização para a média
    mu <- exp(X %*% betas)
    Xb <- nu * log(mu + (nu - 1)/(2 * nu))
    ##-------------------------------------------
    i <- 0:sumto
    zs <- sapply(Xb, function(loglambda) sum(exp(i * loglambda -
                                                 nu * lfactorial(i))))
    Z <- sum(log(zs))
    ll <- sum(y * Xb - nu * lfactorial(y)) - Z
    return(-ll)
}


## ------------------------------------------------------------------------

## Modifica framework para llcmp3
cmp3 <- function (formula, data, start = NULL, sumto = NULL, ...) {
    frame <- model.frame(formula, data)
    terms <- attr(frame, "terms")
    y <- model.response(frame)
    X <- model.matrix(terms, frame)
    ## off <- model.offset(frame)
    ## if (is.null(sumto))
    ##     sumto <- ceiling(max(y)^1.5)
    if (is.null(start)) {
        m0 <- glm.fit(x = X, y = y, family = poisson())
        start <- c(phi = 0, m0$coefficients)
    }
    bbmle::parnames(llcmp3) <- names(start)
    model <- bbmle::mle2(llcmp3, start = start, data = list(y = y,
        X = X), vecpar = TRUE, ...)
    return(model)
}


## ------------------------------------------------------------------------

## Ajustando o modelo
m4C2 <- cmp3(f4, data = capdesfo)
convergencez(m4C2)


## ------------------------------------------------------------------------

## Perfis de verossimilhança para phi
perf <- profile(m4C2, which = "phi")
confint(perf)
plot(perf)


## ------------------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz de variâncias e covariâncias
Vcov <- vcov(m4C2)
Corr <- cov2cor(Vcov)

corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## ------------------------------------------------------------------------

X <- m4C2@data$X
mu <- c(exp(X %*% coef(m4C2)[-1]))

##-------------------------------------------
## Considerando a COM-Poisson reparametrizada para a média
f4; f4[-2]
X <- model.matrix(f4[-2], data = pred)

betas <- coef(m4C2)[-1]
phi <- coef(m4C2)[1]
logmu <- X %*% betas

## Obtendo os erros padrão das estimativas
##   Obs.: Deve-se usar a matriz de variâncias e covariâncias
##   condicional, pois os parâmetros de locação (betas) e dispersão
##   (phi) não são ortogonais.
Vc <- Vcov[-1, -1] - Vcov[-1, 1] %*% solve(Vcov[1, 1]) %*% Vcov[1, -1]
U <- chol(Vc)
se <- sqrt(apply(X %*% t(U), MARGIN = 1, FUN = function(x) {
    sum(x^2)
}))

aux <- exp(c(logmu) + outer(se, qn, FUN = "*"))
aux <- data.frame(modelo = "COM-Poisson", aux)
predC2 <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC2)

update(xy, type = c("p", "g"), key = key, alpha = 0.7) +
    as.layer(xyplot(fit ~ des | est,
                    groups = modelo,
                    data = da,
                    type = "l",
                    ly = da$lwr,
                    uy = da$upr,
                    cty = "bands",
                    alpha = 0.5,
                    prepanel = prepanel.cbH,
                    panel.groups = panel.cbH,
                    panel = panel.superpose))


## ------------------------------------------------------------------------

data(ninfas)
str(ninfas)
## help(ninfas)


## ------------------------------------------------------------------------

## Somente as cultivares que contém BRS na identificação
ninfas <- droplevels(subset(ninfas, grepl("BRS", x = cult)))

## Categorizando a variável dias em aval
ninfas$aval <- factor(ninfas$dias)

str(ninfas)


## ------------------------------------------------------------------------

## Experimento balanceado
xtabs(~aval + cult, data = ninfas)

(xy <- xyplot(ntot ~ aval | cult,
              data = ninfas,
              type = c("p", "g", "smooth"),
              jitter.x = TRUE))

## Avaliando preliminarmente suposição de equidispersão
(mv <- aggregate(ntot ~ data + cult, data = ninfas,
                 FUN = function(x) c(mean = mean(x), var = var(x))))

xlim <- ylim <- extendrange(c(mv$ntot), f = 0.05)
xyplot(ntot[, "var"] ~ ntot[, "mean"],
       data = mv,
       ## xlim = xlim,  ylim = ylim,
       ylab = "Variância Amostral",
       xlab = "Média Amostral",
       panel = function(x, y) {
           panel.xyplot(x, y, type = c("p", "r"), grid = TRUE)
           panel.abline(a = 0, b = 1, lty = 2)
       })


## ---- cache = TRUE-------------------------------------------------------

## Preditores considerados
f1 <- ntot ~ bloco + cult + aval
f2 <- ntot ~ bloco + cult * aval

## Ajustando os modelos Poisson
m1P <- glm(f1, data = ninfas, family = poisson)
m2P <- glm(f2, data = ninfas, family = poisson)

## Ajustando os modelos COM-Poisson
m1C <- cmp(f1, data = ninfas, sumto = 800)
m2C <- cmp(f2, data = ninfas, sumto = 800)


## ------------------------------------------------------------------------

## Verossimilhancas dos modelos ajustados
cbind("Poisson" = sapply(list(m1P, m2P), logLik),
      "COM-Poisson" = sapply(list(m1C, m2C), logLik))

## Teste de razão de verossimilhanças
anova(m1P, m2P, test = "Chisq")
anova(m1C, m2C)


## ------------------------------------------------------------------------

## Estimativas dos parâmetros
summary(m1P)
summary(m1C)


## ------------------------------------------------------------------------

## Um dos problemas computacionais do modelo COM-Poisson é a obtenção da
## constante de normalização Z. Assim uma visualização pós ajuste para
## verificar se o ajuste proporcionou uma densidade válida se faz
## necessária
convergencez(m1C)


## ------------------------------------------------------------------------

## Dado que o modelo COM-Poisson leva as mesmas estimativas pontuais que
## o modelo Poisson a análise de diagnóstico padrão pode ser utilizada
par(mfrow = c(2, 2))
plot(m1P)


## ---- cache = TRUE-------------------------------------------------------

##-------------------------------------------
## Testando a nulidade do parâmetro phi

## Usando o ajuste Poisson
trv <- 2 * (logLik(m1C) - logLik(m1P))
attributes(trv) <- NULL
round(c(trv, pchisq(trv, 1, lower = FALSE)), digits = 5)

## Reajustando o COM-Poisson para phi = 0 (ou equivalente nu = 1)
m1Cfixed <- cmp(f1, data = ninfas, fixed = list("phi" = 0))
anova(m1C, m1Cfixed)


## ----perf4, cache = TRUE, warnings = FALSE-------------------------------

## Via perfil de log-verossimilhança
perf <- profile(m1C, which = "phi")
confint(perf)
plot(perf)


## ------------------------------------------------------------------------

##-------------------------------------------
## Verificando a matriz ve variâncias e covariâncias
Vcov <- vcov(m1C)
Corr <- cov2cor(Vcov)

library(corrplot)
corrplot.mixed(Corr, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8, col = brewer.pal(9, "Greys")[-(1:3)])


## ------------------------------------------------------------------------

## Predição pontual/intervalar
pred <- with(ninfas,
             expand.grid(
                 bloco = factor(levels(bloco)[1],
                                levels = levels(bloco)),
                 cult = levels(cult),
                 aval = levels(aval)
             ))
qn <- qnorm(0.975) * c(fit = 0, lwr = -1, upr = 1)

f1; f1[-2]
X <- model.matrix(f1[-2], data = pred)

## Como não temos interesse na interpretação dos efeitos de blocos
## tomaremos a média desses efeitos para predição

bl <- attr(X, "assign") == 1
X[, bl] <- X[, bl] * 0 + 1/(sum(bl) + 1)
head(X)

library(multcomp)

##-------------------------------------------
## Considerando a Poisson
aux <- exp(confint(glht(m1P, linfct = X),
               calpha = univariate_calpha())$confint)
colnames(aux) <- c("fit", "lwr", "upr")
aux <- data.frame(modelo = "Poisson", aux)
predP <- cbind(pred, aux)

##-------------------------------------------
## Considerando a COM-Poisson
aux <- predict(m1C, newdata = X, type = "response",
               interval = "confidence")
aux <- data.frame(modelo = "COM-Poisson", aux)
predC <- cbind(pred, aux)

##-------------------------------------------
## Visualizando os valores preditos intervalares pelos dois modelos
da <- rbind(predP, predC)
da <- da[order(da$cult, da$aval, da$modelo), ]

source(paste0("https://gitlab.c3sl.ufpr.br/leg/legTools/raw/",
              "issue%2315/R/panel.segplot.by.R"))

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(da$modelo) + 3, lty = 1),
            text = list(c("Poisson", "COM-Poisson")))

update(xy, type = c("p", "g"), key = key, alpha = 0.7) +
    as.layer(segplot(
        aval ~ lwr + upr | cult,
        centers = fit, groups = modelo, data = da,
        grid = TRUE, horizontal = FALSE, draw = FALSE,
        lwd = 2, pch = 1:nlevels(da$modelo) + 3,
        panel = panel.segplot.by, f = 0.1))


