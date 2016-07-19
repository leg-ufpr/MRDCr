## ----setup, include=FALSE------------------------------------------------
source("_setup.R")

## ------------------------------------------------------------------------
library(MRDCr)

## ------------------------------------------------------------------------

data(peixe)
peixe$campista <- as.factor(peixe$campista)
levels(peixe$campista) <- c("Não", "Sim")

str(peixe)
## help(peixe)


## ------------------------------------------------------------------------

## Estudo observacional
ftable(with(peixe, table(npessoas, ncriancas, campista)))

## Resumo das variáveis
summary(peixe)

## Resumo das variáveis, considerando somente as respostas não nulas
summary(subset(peixe, npeixes > 0))

## Contagens (marginal aos efeitos das covariáveis)
p1 <- histogram(~npeixes, data = peixe, nint = 50, grid = TRUE)
p2 <- histogram(~npeixes, data = subset(peixe, npeixes > 0),
                nint = 50)

print(p1, split = c(1, 1, 2, 1), more = TRUE)
print(p2, split = c(2, 1, 2, 1))

## Proporção dos valores observados 
(proptb <- cbind("Proporção" = prop.table(table(peixe$npeixes)),
                 "N. observ" = table(peixe$npeixes)))

## Disposição das covariáveis
par(mfrow = c(1, 3))
sapply(1:3, function(x) {
    barplot(table(peixe[, x]))
    title(main = names(peixe)[x])
    names(peixe)[x]
})

## Contagens com relação as covariáveis
peixe$lnpeixes <- log(peixe$npeixes + 0.5)
xyplot(lnpeixes ~ ncriancas + npessoas,
       groups = campista,
       auto.key = list(
           columns = 2, cex.title = 1, lines = TRUE,
           title = "Presença de campista"),
       data = peixe,
       jitter.x = TRUE,
       type = c("p", "g", "spline"))


## ------------------------------------------------------------------------

##======================================================================
## Ajuste de modelos hurdle
library(pscl)

## Preditores
f1 <- npeixes ~ campista + npessoas + ncriancas
f2 <- npeixes ~ campista * npessoas + ncriancas

## Poisson
m1P <- glm(f1, data = peixe, family = poisson)
m2P <- glm(f2, data = peixe, family = poisson)

## Hurdle Poisson
m1HP <- hurdle(f1, data = peixe, dist = "poisson")
m2HP <- hurdle(f2, data = peixe, dist = "poisson")

## Zero Inflated Poisson
m1ZP <- zeroinfl(f1, data = peixe, dist = "poisson")
m2ZP <- zeroinfl(f2, data = peixe, dist = "poisson")

## Binomial Negativa
library(MASS)
m1BN <- glm.nb(f1, data = peixe)
m2BN <- glm.nb(f2, data = peixe)

## Hurdle Binomial Negativa
m1HBN <- hurdle(f1, data = peixe, dist = "negbin")
m2HBN <- hurdle(f2, data = peixe, dist = "negbin")

## Zero Inflated Binomial Negativa
m1ZBN <- zeroinfl(f1, data = peixe, dist = "negbin")
m2ZBN <- zeroinfl(f2, data = peixe, dist = "negbin")


## ------------------------------------------------------------------------

## Via log-verossimilhança
cbind("Poisson" = sapply(list(m1P, m2P), logLik),
      "HUPoisson" = sapply(list(m1HP, m2HP), logLik),
      "ZIPoisson" = sapply(list(m1ZP, m2ZP), logLik),
      "BinNeg" = sapply(list(m1BN, m2BN), logLik),
      "HUBinNeg" = sapply(list(m1HBN, m2HBN), logLik),
      "ZIBinNeg" = sapply(list(m1ZBN, m2ZBN), logLik)
      )


## ------------------------------------------------------------------------

## Testes de razão de verossimilhanças para o efeito de interação
anova(m1BN, m2BN)
lmtest::lrtest(m1HBN, m2HBN)
lmtest::lrtest(m1ZBN, m2ZBN)


## ------------------------------------------------------------------------

## Teste de Vuong para diferença entre os modelos BN e HUBN
vuong(m1BN, m1HBN)

## Teste de Vuong para diferença entre os modelos ZIBN e HUBN
vuong(m1ZBN, m1HBN)


## ------------------------------------------------------------------------

## Estimativas dos parâmetros e testes de Wald
summary(m1BN)
summary(m1HBN)
summary(m1ZBN)


## ------------------------------------------------------------------------

## Ajuste de preditor do modelo proposto
## Retira o efeito de campista no preditor para as contagens não nula
m3HBN <- hurdle(npeixes ~ npessoas + ncriancas |
                    campista + npessoas + ncriancas,
                data = peixe, dist = "negbin")
lmtest::lrtest(m1HBN, m3HBN)

## Refazendo o teste de Vuong para comparação de modelos BN e HUBN
vuong(m1BN, m3HBN)


## ----diag, cache = TRUE, fig.height = 4----------------------------------

## Uma pequena avaliação dos resíduos 
p1 <- xyplot(residuals(m3HBN) ~ fitted(m3HBN),
             type = c("p", "g", "spline"))

p2 <- qqmath(~residuals(m3HBN, type = "pearson"),
             type = c("p", "g"),
             panel = function(x, ...) {
                 panel.qqmathline(x, ...)
                 panel.qqmath(x, ...)
             })

qqsimul <- hnp::hnp(m3HBN, plot = FALSE)
p3 <- with(qqsimul,
           xyplot(residuals ~ x, pch = 20) +
           as.layer(
               xyplot(median + lower + upper ~ x,
                      type = "l", col = 1, lty = c(2, 1, 1)))
           )

print(p1, split = c(1, 1, 3, 1), more = TRUE)
print(p2, split = c(2, 1, 3, 1), more = TRUE)
print(p3, split = c(3, 1, 3, 1), more = FALSE)


## ------------------------------------------------------------------------

## Estimativas dos parâmetros e testes de Wald
summary(m3HBN)


## ------------------------------------------------------------------------

##----------------------------------------------------------------------

## Região para predição
pred <- expand.grid(campista = c("Não", "Sim"),
                    ncriancas = 0:3, npessoas = 1:4)

##-------------------------------------------
## Estimando as médias
aux <- predict(m3HBN, newdata = pred, type = "response")
predmu <- cbind(pred, fit = aux)

xyplot(fit ~ npessoas | campista,
       groups = ncriancas, data = predmu,
       type = c("l", "g"),
       auto.key = list(
           columns = 2, cex.title = 1,
           lines = TRUE, points = FALSE,
           title = "Número de crianças"),
       strip = strip.custom(
           strip.names = TRUE, var.name = "campista"
       ))

##-------------------------------------------
## Estimando probabilidades
pred <- expand.grid(campista = "Sim",
                    ncriancas = 0:3, npessoas = 4)
py <- c(t(predict(m3HBN, type = "prob", at = 0:20, newdata = pred)))
da <- data.frame(y = 0:20, py = py, ncriancas = rep(0:3, each = 21))

barchart(py ~ y | factor(ncriancas), data = da,
         stack = FALSE, horizontal = FALSE,
         as.table = TRUE, between = list(y = 0.5),
         scales = list( y = "free", x = list(labels = 0:20)),
         strip = strip.custom(
             strip.names = TRUE, var.name = "nº de crianças"
         ))


## ----boot, cache = TRUE--------------------------------------------------

## Intervalos de confiança para predição

##-------------------------------------------
## Via reamostragem
n <- nrow(peixe)
formula <- m3HBN$formula
start <- list(zero = coef(m3HBN, "zero"), count = coef(m3HBN, "count"))

boots <- replicate(100, {
    id <- sample(1:n, replace = TRUE)
    model <- hurdle(formula, data = peixe[id, ], start = start)
    yhat <- predict(model, newdata = predmu, type = "response")    
})

pred2 <- cbind(predmu, t(apply(boots, 1, function(x) {
    quantile(x, probs = c(0.025, 0.975))})))
names(pred2)[5:6] <- c("lwr", "upr")

## Viasualizando graficamente
xyplot(fit ~ npessoas | campista,
       groups = ncriancas, data = pred2,
       type = c("l", "g"), cty = "bands", 
       ly = pred2$lwr, uy = pred2$upr,
       prepanel = prepanel.cbH, alpha = 0.5,
       panel = panel.superpose,
       panel.groups = panel.cbH,
       auto.key = list(
           columns = 2, cex.title = 1,
           lines = TRUE, points = FALSE,
           title = "Número de crianças"),
       strip = strip.custom(
             strip.names = TRUE, var.name = "campista"
         ))


## ------------------------------------------------------------------------

## Modelo Hurdle (binomial e binomial negativa) 
m3HBN$formula

##-------------------------------------------
## Componente da contagem nula (f_zero)
indica <- with(peixe, ifelse(npeixes == 0, 1, 0))
mzero <- glm(indica ~ campista + npessoas + ncriancas,
             family = binomial, data = peixe)

## Comparando os coeficientes
cbind("glm_binomial" = coef(mzero),
      "zeroinfl" = coef(m3HBN, model = "zero"))

##-------------------------------------------
## Componente da contagem nula (f_count)
library(VGAM)
countp <- subset(peixe, npeixes > 0)
mcount <- vglm(npeixes ~ npessoas + ncriancas,
               family = posnegbinomial, data = countp)

## Comparando os coeficientes (betas e theta (da BN))
cbind("vglm_posnegbin" = coef(mcount)[-2],
      "zeroinfl" = coef(m3HBN, model = "count"))
cbind("vglm_posnegbin" = exp(coef(mcount)[2]), 
      "zeroinfl" = m3HBN$theta)


## ------------------------------------------------------------------------

data(seguros)
str(seguros)
## help(seguros)


## ------------------------------------------------------------------------

summary(seguros)


## ------------------------------------------------------------------------

library(pscl)

## Preditores
f0 <- nsinist ~ 1
f1 <- nsinist ~ sexo + valor + log(expos)
f2 <- nsinist ~ sexo * valor + log(expos)

## Poisson
m0P <- glm(f0, data = seguros, family = poisson)
m1P <- glm(f1, data = seguros, family = poisson)
m2P <- glm(f2, data = seguros, family = poisson)

## Hurdle Poisson
m0HP <- hurdle(f0, data = seguros, dist = "poisson")
m1HP <- hurdle(f1, data = seguros, dist = "poisson")
m2HP <- hurdle(f2, data = seguros, dist = "poisson")

## Zero Inflated Poisson
m0ZP <- zeroinfl(f0, data = seguros, dist = "poisson")
m1ZP <- zeroinfl(f1, data = seguros, dist = "poisson")
m2ZP <- zeroinfl(f2, data = seguros, dist = "poisson")

## Binomial Negativa
library(MASS)
m0BN <- glm.nb(f0, data = seguros)
m1BN <- glm.nb(f1, data = seguros)
m2BN <- glm.nb(f2, data = seguros)

## Hurdle Binomial Negativa
m0HBN <- hurdle(f0, data = seguros, dist = "negbin")
m1HBN <- hurdle(f1, data = seguros, dist = "negbin")
m2HBN <- hurdle(f2, data = seguros, dist = "negbin")

## Zero Inflated Poisson
m0ZBN <- zeroinfl(f0, data = seguros, dist = "negbin")
m1ZBN <- zeroinfl(f1, data = seguros, dist = "negbin")
m2ZBN <- zeroinfl(f2, data = seguros, dist = "negbin")


## ------------------------------------------------------------------------

## Via log-verossimilhança
cbind("Poisson" = sapply(list(m0P, m1P, m2P), logLik),
      "HUPoisson" = sapply(list(m0HP, m1HP, m2HP), logLik),
      "ZIPoisson" = sapply(list(m0ZP, m1ZP, m2ZP), logLik),
      "BinNeg" = sapply(list(m0BN, m1BN, m2BN), logLik),
      "HUBinNeg" = sapply(list(m0HBN, m1HBN, m2HBN), logLik),
      "ZIBinNeg" = sapply(list(m0ZBN, m1ZBN, m2ZBN), logLik)
      )


## ------------------------------------------------------------------------

## Testes de razão de verossimilhanças
lmtest::lrtest(m0HP, m1HP, m2HP)

lmtest::lrtest(m0HBN, m1HBN, m2HBN)


## ------------------------------------------------------------------------

## Para a componente de contagens não negativas é necessário um modelo
## Binomial Negativa, considerando superdispersão dos dados?
vuong(m1HP, m1HBN)


## ------------------------------------------------------------------------

## Estimativas do modelo proposto
summary(m1HP)

## Retira efeito de sexo na componente das contagens nulas
m3HP <- hurdle(nsinist ~ 1 | sexo + valor + log(expos),
               data = seguros)

lmtest::lrtest(m1HP, m3HP)

summary(m3HP)


## ------------------------------------------------------------------------

## Avaliação de diferentes especificações para a componente de contagens
## nulas 
m3HP.pois <- hurdle(nsinist ~ 1 | sexo + valor + log(expos),
                    data = seguros, zero.dist = "poisson")

m3HP.negb <- hurdle(nsinist ~ 1 | sexo + valor + log(expos),
                    data = seguros, zero.dist = "negbin")

m3HP.geom <- hurdle(nsinist ~ 1 | sexo + valor + log(expos),
                    data = seguros, zero.dist = "geometric")

## Comparação das funções de ligação
sapply(list("binomial" = m3HP, "poisson" = m3HP.pois,
            "negbin" = m3HP.negb, "geometric" = m3HP.geom), logLik)


## ------------------------------------------------------------------------

## Região para predição
pred <- expand.grid(sexo = c("M", "F"),
                    valor = 1:200,
                    expos = c(0.1, 0.5, 1))
##-------------------------------------------
## Estimando as médias
aux <- predict(m3HP, newdata = pred, type = "response")
predmu <- cbind(pred, fit = aux)

xyplot(fit ~ valor | factor(expos),
       layout = c(NA, 1),
       groups = sexo,
       data = predmu,
       type = c("l", "g"),
       strip = strip.custom(strip.names = TRUE,
                            var.name = "Exposição"),
       auto.key = list(
           columns = 2, cex.title = 1,
           lines = TRUE, points = FALSE,
           title = "Número de crianças"))


## ------------------------------------------------------------------------

##-------------------------------------------
## Estimando probabilidades
pred <- expand.grid(sexo = c("M", "F"),
                    valor = c(1, 50),
                    expos = 0.5)

py <- c(t(predict(m3HP, type = "prob", at = 0:5, newdata = pred)))
carac <- with(pred, paste("Sexo:", sexo, "Valor:", valor))
da <- data.frame(y = 0:5, py = py,
                 sexo = rep(c("M", "F"), each = 6),
                 valor = rep(c(1, 50), each = 12))

useOuterStrips(
    barchart(py ~ y | sexo + factor(valor), data = da,
             stack = FALSE, horizontal = FALSE,
             as.table = TRUE, between = list(y = 0.5),
             scales = list( y = "free", x = list(labels = 0:5))
             ),
    strip = strip.custom(strip.names = TRUE, var.name = "Sexo"),
    strip.left = strip.custom(strip.names = TRUE, var.name = "Valor")
)


