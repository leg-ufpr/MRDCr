#' @importFrom stats qnorm
NULL

#' @name calc_mean_gcnt
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Calcula o Valor Esperado para a Distribui\enc{çã}{ca}o Gamma Count
#' @description Função para calcular a média do tipo \eqn{E(Y) = \mu =
#'     \sum y\cdot \Pr(y)} para uma variável aleatória Gamma Count a
#'     partir dos parâmetros \eqn{\lambda > 0} e \eqn{\alpha > 0}.
#' @param lambda Parâmetro de locação da distribuição Gamma Count,
#'     \eqn{\lambda > 0}. Quando \eqn{\alpha = 1}, o parâmetro
#'     \eqn{\lambda = E(Y)} é a média.
#' @param alpha Parâmetro de dispersão da distribuição Gamma Count,
#'     \eqn{\alpha > 0}.
#' @param tol Tolerância para interromper a procura pelo valor de
#'     \code{ymax}, valor cuja probabilidade correspondente é inferior a
#'     \code{tol}, para valores os valores de \code{lambda} e
#'     \code{alpha} informados.
#' @return Um vetor de tamanho igual ao do maior vetor, \code{lambda} ou
#'     \code{alpha} com os valores correspondentes de \eqn{\mu}.
calc_mean_gcnt <- function(lambda, alpha, tol = 1e-5) {
    # Faz com que os parâmetros sejam vetores de mesmo tamanho.
    names(lambda) <- NULL
    names(alpha) <- NULL
    pars <- data.frame(lambda = lambda, alpha = alpha)
    # Calcula o ymax usando mu + 5 (sqrt(sigma))
    ymax <- with(pars, ceiling(max(lambda + 5 * sqrt(lambda/alpha))))
    # Agora verifica se a prob(ymax) é de fato pequena, se não, soma 1.
    lambdamax <- max(pars$lambda)
    alphamin <- max(pars$alpha)
    pmax <- dgcnt(y = ymax, lambda = lambdamax, alpha = alphamin)
    while (pmax > tol) {
        ymax <- ymax + 1
        pmax <- dgcnt(y = ymax, lambda = lambdamax, alpha = alphamin)
    }
    # Define o vetor onde avaliar a acumulada da Gamma.
    y <- 1:ymax
    estmean <- mapply(lambda = pars$lambda,
                      alpha = pars$alpha,
                      MoreArgs = list(y = y),
                      FUN = function(lambda, alpha, y) {
                          sum(pgamma(q = 1,
                                     shape = y * alpha,
                                     rate = alpha * lambda))
                      },
                      SIMPLIFY = TRUE)
    names(estmean) <- NULL
    return(estmean)
}

# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
# @description Função para obter o valores de \eqn{\eta = X\beta} que
#     é o preditor da parte de locação do modelo de regressão,
#     incluindo o intervalo de confiança para \eqn{\eta}, caso
#     corretamente especificado pelo argumento \code{qn}.
# @param V Matriz de variância e covariância das estimativas dos
#     parâmetros da parte de locação do modelo, necessário para
#     calcular a média.
# @param X Matriz de funções lineares para obter \eqn{\hat{eta} = X
#     \hat{\beta}}, em que \eqn{\beta} são os coeficientes da porção de
#     locação.
# @param b Estimativas dos parâmetros da parte de locação, ou seja,
#     \eqn{\hat{\beta}}.
# @param qn Quantis da distribuição normal padrão apropriados para um
#     intervalo de confiança conhecida.
# @return Um vetor se \code{length(qn) == 1} e uma matriz caso
#     contrário.
cholV_eta <- function(V, X, b, qn) {
    # V: matriz de covariância dos parâmetros de média.
    # X: matriz funções lineares para obter as médias.
    # b: vetor de estimativas dos parâmetros de média.
    # qn: quantis da normal para o nível de confiança informado.
    eta <- c(X %*% b)
    if (length(qn) > 1) {
        U <- chol(V)
        stderr <- sqrt(apply(X %*% t(U),
                             MARGIN = 1,
                             FUN = function(x) {
                                 sum(x^2)
                             }))
        eta <- sweep(x = outer(stderr, qn, FUN = "*"),
                     MARGIN = 1,
                     STATS = eta,
                     FUN = "+")
    }
    return(eta)
}

# @author Walmes Zeviani, \email{walmes@@ufpr.br}
# @description Função para fazer a predição para modelos de regressão
#     Poisson Generalizado, Gamma Count e COM-Poisson. Essa função
#     recebe o objeto retornado pela \code{\link[bbmle]{mle2}} e, de
#     acordo com a função de log-verossimilhança usada na otimização
#     (\code{\link{llpgnz}}, \code{\link{llgcnt}} ou \code{llcmp}),
#     retorna os valores preditos na escala do preditor linear ou da
#     resposta.
# @usage
# predict(object,
#         newdata,
#         type = c("link", "response"),
#         interval = c("none", "confidence"),
#         level = 0.95, ...)
# @param object Um objeto de classe \code{mle2} proveniente da função
#     \link[bbmle]{mle2} ou das funções \code{\link{pgnz}},
#     \code{\link{gcnt}} ou \code{cmp}.
# @param newdata Uma matriz com colunas da dimensão do número de
#     parâmetro na parte de média do modelo de regressão para
#     contagem. O argumento \code{newdata} nesta função não lida com
#     \code{data.frame}.
# @param type Tipo de valor retornado, em que \code{"link"} é na
#     escala do preditor linear e \code{"response"} é na média da
#     distribuição da variável de contagem.
# @param interval Especifica o tipo de intervalo de confiança calculado
#     em que \code{"confidence"} produz intervalo de confiança para o
#     tipo de valor retornado e \code{"none"} apenas retorna as
#     estimativas pontuais.
# @param level Nível de confiança do intervalo, quando não informado
#     0.95 é usado.
# @inheritParams stats::predict
# @return Um vetor se \code{interval = "none"} e uma matriz de 3
#     colunas se \code{interval = "confidence"}
# @importFrom stats qnorm
# @examples
#
# str(capdesfo)
# n0 <- gcnt(ncap ~ est * (des + I(des^2)), data = capdesfo)
# n1 <- pgnz(ncap ~ est * (des + I(des^2)), data = capdesfo)
#
# cbind(predict(n0, type = "response"),
#       predict(n1, type = "response"))
#
# Define uma função método.
predict.mle2 <- function(object, newdata,
                         type = c("link", "response"),
                         interval = c("none", "confidence"),
                         level = 0.95, ...) {
    #----------------------------------------
    if (!missing(newdata)) {
        if (is.matrix(newdata)) {
            if (all(colnames(newdata) == names(coef(object)[-1]))) {
                X <- newdata
            } else {
                stop(paste("Nomes das colunas em `newdata` nao",
                           "bate com dos coeficientes."))
            }
        } else {
            stop("`newdata` tem que ser uma matriz.")
        }
    } else {
        X <- object@data$X
    }
    #--------------------------------------------
    interval <- match.arg(interval)
    qn <- -qnorm((1 - level[1])/2)
    qn <- switch(interval[1],
                 confidence = qn * c(lwr = -1, fit = 0, upr = 1),
                 none = qn * c(fit = 0))
    #--------------------------------------------
    cll <- as.character(object@call.orig)
    mdl <- grep(x = cll,
                pattern = "\\b(llpgnz|llgcnt|llcmp)\\b",
                value = TRUE)
    if (!mdl %in% c("llpgnz", "llgcnt", "llcmp")) {
        stop("Funcao usada como `minuslogl` nao reconhecida.")
    }
    #--------------------------------------------
    type <- match.arg(type)
    pred <-
        switch(mdl,
               "llpgnz" = {
                   V <- vcov(object)
                   V <- V[-1, -1]
                   eta <- cholV_eta(V, X,
                                    b = coef(object)[-1],
                                    qn = qn)
                   switch(type,
                          "link" = eta,
                          "response" = exp(eta))
               },
               "llgcnt" = {
                   V <- vcov(object)
                   V <- V[-1, -1]
                   eta <- cholV_eta(V, X,
                                    b = coef(object)[-1],
                                    qn = qn)
                   switch(type,
                          "link" = eta,
                          "response" = {
                              apply(exp(as.matrix(eta)),
                                    MARGIN = 2,
                                    FUN = calc_mean_gcnt,
                                    alpha = exp(coef(object)[1]))})
               },
               "llcmp" = {
                   V <- vcov(object)
                   Vc <- V[-1, -1] - V[-1, 1] %*%
                       solve(V[1, 1]) %*% V[1, -1]
                   eta <- cholV_eta(Vc, X,
                                    b = coef(object)[-1],
                                    qn = qn)
                   switch(type,
                          "link" = eta,
                          "response" = {
                              apply(exp(as.matrix(eta)),
                                    MARGIN = 2,
                                    FUN = calc_mean_cmp,
                                    nu = exp(coef(object)[1]),
                                    sumto = object@data$sumto)})
               })
    pred <- cbind(pred)
    colnames(pred) <- names(qn)
    return(pred)
}

# @author Walmes Zeviani, \email{walmes@@ufpr.br}
# @description Apenas para criar o método \code{predict} para objetos
#     retornados pela \code{\link[bbmle]{mle2}}. No entanto, essa
#     função só irá retornar predição se as funções otimizadas forem as
#     log-verossimilhanças \code{\link{llpgnz}}, \code{\link{llgcnt}} e
#     \code{llcmp}.
methods::setMethod(f = "predict",
                   signature = "mle2",
                   definition = predict.mle2)
