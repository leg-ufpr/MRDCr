## ----setup, include=FALSE------------------------------------------------
source("_setup.R")

## ---- results = "hide", message = FALSE----------------------------------
# Pacotes necessários.
library(lattice)
library(MASS)
library(effects)
library(knitr)
library(MRDCr)

## ------------------------------------------------------------------------
# Dez primeiras linhas da base.
head(seguros, 10)
str(seguros)

seguros$lexpo <- log(seguros$expos)

## ------------------------------------------------------------------------
# Distribuição do números de sinistros.
tb <- table(seguros$nsinist)
tb

barchart(tb, horizontal = FALSE)

# Taxa de sinistros na amostra.
taxageral <- sum(seguros$nsinist)/sum(seguros$expos)
taxageral

tab <- aggregate(cbind(expos, nsinist) ~ sexo,
                 data = seguros, FUN = sum)
taxa <- with(tab, nsinist/expos)
tab <- cbind(tab, taxa)

# Distribuição do número de sinistros por sexo.
kable(tab, align = "c",
      caption = "**Taxa de sinistros segundo sexo**")

seguros$idadecat <- cut(seguros$idade,
                       breaks = c(18, 30, 60, 92),
                       include.lowest = TRUE)
tab <- aggregate(cbind(expos, nsinist) ~ idadecat,
                 data = seguros, FUN = sum)
taxa <- with(tab, nsinist/expos)
tab <- cbind(tab, taxa)

# Distribuição do número de sinistros por sexo.
kable(tab, align = "c",
      caption = "**Taxa de sinistros segundo idade**")

tabidsex <- aggregate(cbind(expos, nsinist) ~ sexo + idadecat,
                      data = seguros, FUN = sum)
taxa <- with(tabidsex, nsinist/expos)
tabidsex <- cbind(tabidsex, taxa)

# Distribuição do número de sinistros por idade e sexo.
kable(tabidsex, align = "c",
      caption = "**Taxa de sinistros segundo sexo e idade**")

## ------------------------------------------------------------------------
seguros <- na.omit(seguros)
mP <- glm(nsinist ~ sexo + idade + I(idade^2) + valor +
              offset(log(expos)),
          data = seguros, family = poisson)
summary(mP)

# Estimação do parâmetro de dispersão.
X2 <- sum(resid(mP, type = "pearson")^2)
phichap <- X2/mP$df.residual

# Indicador de superdispersão.
phichap

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# Diagnóstico do modelo - gráficos.
par(mfrow = c(2, 2))
plot(mP)

par(mfrow = c(1, 1))
envelope(mP)

## ------------------------------------------------------------------------
mPo <- glm(nsinist ~ sexo + idade + I(idade^2) + valor +
               log(expos),
           data = seguros, family = poisson)
summary(mPo)
anova(mP, mPo, test = "Chisq")

## ------------------------------------------------------------------------
mBNo <- glm.nb(nsinist ~ sexo + idade + I(idade^2) + valor +
                   log(expos), data = seguros)
summary(mBNo)

## ------------------------------------------------------------------------
# Diagnóstico do modelo - gráficos.
par(mfrow = c(2, 2))
plot(mBNo)

## ------------------------------------------------------------------------
dadosnb3 <-
    seguros[, c("sexo", "idade", "valor", "expos", "nsinist")]
dadosnb3$lexpo <- log(seguros$expos)

mBNo <- glm.nb(nsinist ~ sexo + idade + I(idade^2) +
                   valor + lexpo,
               data = dadosnb3)

envelope <- function(modelo) {
    dados <- na.omit(dadosnb3)
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

## ------------------------------------------------------------------------
par(mfrow = c(1, 1))
envelope(mBNo)

## ------------------------------------------------------------------------
intervalos <- confint(mBNo)
estimat <- cbind(mBNo$coefficients, intervalos)
colnames(estimat)[1] <- "Estimativa pontual"

# Quadro de estimativas
kable(round(estimat, 5), align = "c",
      caption = paste("**Estimativas pontuais e intervalos de",
                      "confiança - Modelo Binomial Negativo**"))

## ------------------------------------------------------------------------
efeitos <- allEffects(mBNo, given.values = c(lexpo = 0))

plot(efeitos[[2]],
     type = "response",
     main = "Taxa de sinistros vs. idade",
     xlab = "Idade (anos)",
     ylab = "Taxa de sinistros")

plot(efeitos[[1]],
     type = "response",
     main = "Taxa de sinistros vs. sexo",
     xlab = "Sexo",
     ylab = "Taxa de  sinistros")

plot(efeitos[[4]],
     type = "response",
     main = "Taxa de sinistros vs. valor do automóvel",
     xlab = "Valor (x1000 reais)",
     ylab = "Taxa de sinistros")

dev.off()

## ------------------------------------------------------------------------

# Poisson sem ajuste de covariáveis.
n <- nrow(seguros)
mediasin <- mean(seguros$nsinist)
freqps <- round(n * dpois(0:10, mediasin))

# Poisson com covariaveis
pred1 <- predict(mPo, type = "response")
intervalo <- 0:10
matprob <- matrix(0, nrow = nrow(seguros), ncol = length(intervalo))
probpois <- function(interv, taxa) dpois(intervalo, taxa)
for (i in 1:nrow(seguros)) {
    matprob[i, ] <- probpois(interv = intervalo, taxa = pred1[i])
}
pbarra <- colMeans(matprob)
freqpsaj <- round(n * pbarra)

# Binomial negativa sem covariaveis.
ajustenb <- glm.nb(nsinist ~ 1, data = seguros)

media <- exp(coefficients(ajustenb))
shape <- ajustenb$theta
freqbn <- round(n * dnbinom(0:10, mu = media, size = shape))

# Binomial negativa com covariaveis
pred2 <- predict(mBNo, type = "response")

intervalo <- 0:10
matprob <- matrix(0, nrow = nrow(seguros), ncol = length(intervalo))
probnb <- function(interv, media, shape) {
    dnbinom(intervalo, mu = media,
            size = shape)
}
for (i in 1:nrow(seguros)) {
    matprob[i, ] <- probnb(interv = intervalo, media = pred2[i],
                           shape = mBNo$theta)
}
pbarra <- colMeans(matprob)
frebnaj <- round(n * pbarra)
ams <- c(table(seguros$nsinist), rep(0, 5))
matfreq <- rbind(ams, freqps, freqpsaj, freqbn, frebnaj)
colnames(matfreq) <- 0:10
rownames(matfreq) <- c("Amostra",
                       "Poisson não ajustada por covariáveis",
                       "Poisson ajustada por covariáveis",
                       "BN não ajustada por covariáveis",
                       "BN ajustada por covariáveis")

## ---- results="markup"---------------------------------------------------
kable(matfreq, format = "markdown",
      caption = paste("Frequências amostrais e frequências",
                      "ajustadas para o número de sinistros"))

## ---- echo=FALSE, results="hold"-----------------------------------------
cat(format(Sys.time(),
           format = "Atualizado em %d de %B de %Y.\n\n"))
sessionInfo()

## ---- include=FALSE------------------------------------------------------
detach("package:effects")

