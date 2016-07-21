## ----setup, include=FALSE-----------------------------------------
source("_setup.R")

## ---- echo=FALSE, include=FALSE-----------------------------------
devtools::load_all()

## ---- results="hide", message=FALSE-------------------------------
# Pacotes requeridos.
library("lmtest")
library("boot")
library("car")
library("latticeExtra")
library("RColorBrewer")
library("sandwich")
library("hnp")
library("knitr")

## -----------------------------------------------------------------
str(postura)
summary(postura)

# names(postura) <- c("npost", "trat", "linh")
# postura <- postura[, c(2, 3, 1)]
# use_data(postura, overwrite = TRUE)
# tab <- xtabs(~trat + npost, data = postura)

bwplot(npost ~ linh | trat,
       data = postura,
       main = "Mudanças de postura vs tratamento e linhagem",
       xlab = "Linhagem",
       ylab = "Frequência",
       pch = "|")

xyplot(npost ~ linh | trat,
       data = postura,
       main = "Mudanças de postura vs tratamento e linhagem",
       xlab = "Linhagem",
       ylab = "Frequência",
       panel = panel.beeswarm, spread = 0.05)

# Variância e média amostrais por trat e linhagem.
mdp <- aggregate(npost ~ trat + linh,
                 data = postura,
                 FUN = function(x) {
                     c(mean = mean(x), var = var(x))
                 })
mdp

## -----------------------------------------------------------------
# Ajuste do modelo Poisson.
mP <- glm(npost ~ trat + linh,
                data = postura,
                family = poisson)
summary(mP)

# Avaliando possível efeito de interação.
mPi <- glm(npost ~ trat * linh,
           data = postura,
           family = poisson)
anova(mP, mPi, test = "Chisq")

exp(coef(mP)[2])
# Estima-se que a taxa de variação de postura no grupo com intervenção
# seja aproximadamente a metade em relação ao grupo sem intervenção.

# Estimação do parâmetro de dispersão.
X2 <- sum(resid(mP, type = "pearson")^2)
phichap <- X2/df.residual(mP)
phichap

## -----------------------------------------------------------------
# Diagnóstico do modelo - gráficos padrão do R.
par(mfrow = c(2, 2))
plot(mP)

## -----------------------------------------------------------------
envelope <- function(modelo) {
    dados <- na.omit(modelo$data)
    nsim <- 100
    n <- modelo$df.null + 1
    r1 <- sort(rstandard(modelo, type = "deviance"))
    m1 <- matrix(0, nrow = n, ncol = nsim)
    a2 <- simulate(modelo, nsim = nsim)

    for (i in 1:nsim) {
        dados$y <- a2[, i]
        aj <- update(modelo, y ~ ., data = dados)
        m1[, i] <- sort(rstandard(aj, type = "deviance"))
    }

    li <- apply(m1, 1, quantile, 0.025)
    m <- apply(m1, 1, quantile, 0.5)
    ls <- apply(m1, 1, quantile, 0.975)

    quantis <- qnorm((1:n - 0.5)/n)

    plot(rep(quantis, 2), c(li, ls), type = "n",
         xlab = "Percentil da N(0,1)",
         ylab = "Resíduos")
    title("Gráfico Normal de Probabilidades")
    lines(quantis, li, type = "l")
    lines(quantis, m, type = "l", lty = 2)
    lines(quantis, ls, type = "l")
    points(quantis, r1, pch = 16, cex = 0.75)
}

## -----------------------------------------------------------------
# Gráfico quantil-quantil com envelopes simulados.
layout(1)
envelope(mP)

## -----------------------------------------------------------------
# Modelo quasi poisson (V(mu) = mu).
mQP0 <- glm(npost ~ trat + linh,
            data = postura,
            family = "quasipoisson")
# summary(mQP0)

# Forma alternativa de declarar o Modelo quase-poisson (V(mu) = mu).
mQP0 <- glm(npost ~ trat + linh,
                data = postura,
                family = quasi(variance = "mu", link = "log"))
# summary(mQP0)

# Modelo de quase-verossimilhança (V(mu) = mu^2).
mQP1 <- glm(npost ~ trat + linh,
                data = postura,
                family = quasi(variance = "mu^2", link = "log"))
summary(mQP1)

# Gráficos de diagnóstico para o modelo de quase-verossimilhança.
par(mfrow = c(2,2))
plot(mQP1)

# Gráficos quantil-quantil para os resíduos dos modelos Poisson e de
# Quase-Verossimilhança.
par(mfrow = c(1, 2))
qqnorm(resid(mP, type = "deviance"),
       pch = 20, main = "Poisson", las = 1)
qqline(resid(mP, type = "deviance"))
qqnorm(resid(mQP1, type = "deviance"),
       pch = 20, main = "QL", las = 1)
qqline(resid(mQP1, type = "deviance"))

## -----------------------------------------------------------------
# Estimador sanduíche.
estrb <- coeftest(mP, vcov = sandwich)
estrb

# Usando bootstrap (R = 999 simulações)
mB <- Boot(mP)

# Resultados obtidos via bootstrap.
summary(mB)

## -----------------------------------------------------------------
erroz <- rbind(summary(mP)$coefficients[2,2:3],
               summary(mQP0)$coefficients[2,2:3],
               summary(mQP1)$coefficients[2,2:3],
               estrb[2,2:3],
               c(summary(mB)[2,4],
                 mean(mB$t[,2]/summary(mB)[2,4])))

ics <- rbind(confint.default(mP)[2,],
             confint.default(mQP0)[2,],
             confint.default(mQP1)[2,],
             estrb[2,1] + c(-1.96,1.96) * estrb[2,2],
             mean(mB$t[, 2]) +
             c(-1.96, 1.96) * summary(mB)[2, 4])

quadres <- cbind(erroz, ics)

rownames(quadres) <- c("Poisson", "Quasi(mu)", "Quasi(mu^2)",
                       "Robusto (sanduiche)", "Bootstrap")

# Quadro resumo para as estimativas produzidas pelos quatro modelos para
# o efeito de intervenção.
kable(quadres,
      format = "markdown",
      caption = "Comparativo dos modelos ajustados")

## -----------------------------------------------------------------
postura.ex <- postura[-c(8, 18, 28), ]

# Ajustando o modelo Poisson sem as três observações.
mPe <- glm(npost ~ trat + linh,
           data = postura.ex,
           family = poisson)

# Estimativa do parâmetro de dispersão.
sum(resid(mPe, type = "pearson")^2)/mPe$df.residual

# Gráficos de diagnóstico.
par(mfrow = c(2, 2))
plot(mPe)

# Agora, o modelo quase-poisson.
mQPe <- glm(npost ~ trat + linh,
            data = postura.ex,
            family = quasi(variance = "mu", link = "log"))

# Estimativas produzidas pelo modelo quasipoisson com e sem as três
# observações.
c1 <- compareCoefs(mP,
                   mQP0,
                   mPe,
                   mQPe,
                   print = FALSE)
colnames(c1) <- c("Est. Poisson c/out", "E.P. Poisson c/out",
                  "Est. Quasi c/out", "E.P. Quasi c/out",
                  "Est. Poisson s/out", "E.P. Poisson s/out",
                  "Est. Quasi s/out", "E.P. Quasi s/out")
kable(round(c1, 3), align = "c")

# Efeito da intervenção desconsiderando as três observações.
exp(coef(mPe)[2])

# O efeito de intervenção aumenta, e torna-se mais significativo,
# mediante exclusão dos outliers.

## ---- echo=FALSE, results="hold"----------------------------------
cat(format(Sys.time(),
           format = "Atualizado em %d de %B de %Y.\n\n"))
sessionInfo()

