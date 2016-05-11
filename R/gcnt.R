#' @title Log-Verossimilhança do Modelo Gamma Count
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @description Calcula a log-verossimilhança de um modelo de regressão
#'     Gamma Count para a resposta de contagem (y).
#' @details A função de verossimilhança para uma observação, \eqn{L}, da
#'     Gamma Count é:
#'
#' \deqn{L(\lambda, \alpha; y) =
#'   \left(\int_{0}^{T}
#'   \frac{(\alpha\lambda)^{y\alpha}}{\Gamma(y\alpha)}\,
#'   u^{y\alpha-1} \exp\{-\alpha\lambda u\}\, \textrm{d}u \right)
#'   - \left(\int_{0}^{T}
#'   \frac{(\alpha\lambda)^{y\alpha}}{\Gamma((y+1)\alpha)}\,
#'   u^{(y+1)\alpha-1} \exp\{-\alpha\lambda u\}\, \textrm{d}u \right).}
#'
#' Se \eqn{\tau \sim \textrm{Gamma}(\alpha, \alpha\lambda)}, então
#'     \eqn{E(\tau) = \frac{\alpha}{\alpha\lambda} = \frac{1}{\lambda}}
#'     e \eqn{V(\tau) = \frac{1}{\alpha\lambda^2}}.
#'
#' Usando \eqn{G()} para representar o resultado de cada uma das
#'     integrais entre parenteses, que correponde a probabilidade
#'     acumulada de uma variável aleatória gamma com parâmetros
#'     \eqn{y\alpha} e \eqn{\alpha\lambda}, tem-se
#'
#' \deqn{L(\lambda, \alpha; y) =
#'   G(T, y\alpha, \alpha\lambda)
#'   - G(T, (y+1)\alpha, \alpha\lambda).}
#'
#' A função log-verossimilhança de uma observação é, portanto,
#'
#' \deqn{\ell(\lambda, \alpha; y) =
#'   \ln \left[ G(T, y\alpha, \alpha\lambda) -
#'   G(T, (y+1)\alpha, \alpha\lambda) \right].}
#'
#' Para uma amostra aleatória independente, a função de
#'     log-verossimilhança é
#'
#' \deqn{\ell(\lambda, \alpha; y) =
#'   \sum_{i=1}^{n} \ln \left[ G(T, y\alpha, \alpha\lambda) -
#'   G(T, (y+1)\alpha, \alpha\lambda) \right].}
#'
#' Nestas expressões, \eqn{\alpha} é o parâmetro de dispersão da
#'     variável aleatória \eqn{Y} sendo que se \eqn{\alpha = 1} então
#'     \eqn{Y \sim \textrm{Poisson}}, se \eqn{\alpha < 1} então
#'     \eqn{V(Y) > E(Y)} e \eqn{\alpha > 1} então \eqn{V(Y) < E(Y)}.
#'
#' Como \eqn{\alpha} e \eqn{\lambda} devem ser positivos, usou-se a
#'     função de ligação log para escrever a log-verossimilhança com
#'     domínio nos reais para os parâmetros.
#' @param params Um vetor de (estimativas dos) parâmetros do modelo de
#'     regressão. O primeiro elemento desse vetor é o parâmetro de
#'     dispersão do modelo e os restantes são parâmetros de
#'     locação. Essa função retorna o negativo da log-verossimilhança
#'     pois foi construída para ser usada na \code{\link[bbmle]{mle2}}.
#' @param y Um vetor com variável dependente do modelo, resposta do tipo
#'     contagem.
#' @param X A matriz de delineamento correspondente ao modelo linear
#'     ligado à média pela função de ligação log. A matriz do modelo
#'     pode ser construída com a função
#'     \code{\link[stats]{model.matrix}}.
#' @param offset Um vetor, de mesmo comprimento de \code{y}, com valores
#'     que correspondem aos offsets (ou exposição) para cada valor
#'     observado. Se \code{NULL}, é usado 1 como offset.
#' @return O negativo da log-verossimilhança do modelo Gamma Count para
#'     os parâmetros e dados informados.
#' @seealso \code{\link[bbmle]{mle2}}.
#' @examples
#'
#' set.seed(123)
#' y <- rpois(10, lambda = 5)
#'
#' # Log-verossimilhança pela Poisson.
#' sum(dpois(y, lambda = 5, log = TRUE))
#'
#' # Log-verossimilhança pela GCNT usando alpha = 0
#' llgcnt(params = c(0, log(5)), y = y, X = cbind(y * 0 + 1))
#'
#' set.seed(121)
#' y <- rpois(100, lambda = exp(1))
#' X <- cbind(0 * y + 1)
#'
#' grid <- expand.grid(alpha = seq(-0.5, 0.5, by = 0.02),
#'                     lambda = seq(0.1, 2.1, by = 0.05))
#' str(grid)
#'
#' grid$ll <- apply(grid, MARGIN = 1,
#'                  FUN = function(vec) {
#'                      llgcnt(params = vec, y = y, X = X,
#'                             offset = NULL)
#'                  })
#'
#' library(latticeExtra)
#'
#' levelplot(ll ~ alpha + lambda, data = grid) +
#'     layer(panel.abline(v = 0, h = 1, lty = 2))
#'
#' @importFrom stats pgamma
llgcnt <- function(params, y, X, offset = NULL) {
    # params: vetor de parâmetros;
    #   params[1]: parâmetro de dispersão (alpha);
    #   params[-1]: parâmetro de locação (lambda);
    # y: variável resposta (contagem);
    # X: matriz do modelo linear;
    # offset: tamanho do domínio onde y foi medido (exposição);
    #----------------------------------------
    if (is.null(offset)) {
        offset <- 1L
    }
    alpha <- exp(params[1])
    eXb <- exp(X %*% params[-1])
    alpha.eXb <- alpha * eXb
    alpha.y <- alpha * y
    # returns the log-likelihood.
    ll <- -sum(log(pgamma(offset,
                          shape = alpha.y,
                          rate = alpha.eXb) -
                   pgamma(offset,
                          shape = alpha.y + alpha,
                          rate = alpha.eXb)))
    # Negativo da log-likelihood.
    return(ll)
}

#' @title Probabilidades do Modelo Gamma Count
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @description Calcula as probabilidades para uma variável aleatória
#'     com distribuição Gamma Count:
#'
#' \deqn{p(y,\lambda,\alpha) =
#'   \left(\int_{0}^{1}
#'   \frac{(\alpha\lambda)^{y\alpha}}{\Gamma(y\alpha)}\,
#'   u^{y\alpha-1}
#'   \exp\{-\alpha\lambda u\}\, \textrm{d}u \right)
#'   - \left(\int_{0}^{1}
#'   \frac{(\alpha\lambda)^{y\alpha}}{\Gamma((y+1)\alpha)}\,
#'   u^{(y+1)\alpha-1}
#'   \exp\{-\alpha\lambda u\}\, \textrm{d}u \right),}
#'
#' em que \eqn{\lambda > 0} é a média da variável aleatória tempo entre
#'     eventos e \eqn{\alpha > 0} é o parâmetro de dispersão.
#' @param y Valor da variável de contagem.
#' @param lambda Valor do parâmetro \eqn{\lambda} que é a média da
#'     distribuição do tempo entre eventos.
#' @param alpha Valor do parâmetro \eqn{\alpha} que é o parâmetro de
#'     dispersão.
#' @return Retorna uma probabilidade, ou seja \eqn{\Pr(Y = y | \lambda,
#'     \alpha) = p(y, \lambda, \alpha)}.
#' @examples
#'
#' dpois(5, lambda = 5)
#' dgcnt(5, lambda = 5, alpha = 1)
#'
#' probs <- data.frame(y = 0:30)
#' within(probs, {
#'     py0 <- dpois(y, lambda = 15)
#'     py1 <- dgcnt(y, lambda = 15, alpha = 1)
#'     py2 <- dgcnt(y, lambda = 15, alpha = 0.5)
#'     py3 <- dgcnt(y, lambda = 15, alpha = 1.5)
#'     plot(py0 ~ y, type = "h",
#'          ylim = c(0, max(c(py0, py2, py3))),
#'          ylab = expression(Pr(Y == y)))
#'     points(y + 0.1, py1, type = "h", col = 2)
#'     points(y - 0.3, py2, type = "h", col = 3)
#'     points(y + 0.3, py3, type = "h", col = 4)
#'     legend("topleft", bty = "n",
#'            col = c(1:4), lty = 1,
#'            legend = expression(
#'                Poisson(lambda == 15),
#'                GC(lambda == 15, alpha == 1),
#'                GC(lambda == 15, alpha == 0.5),
#'                GC(lambda == 15, alpha == 1.5)))
#' })
#'
#' @importFrom stats pgamma
dgcnt <- function(y, lambda, alpha) {
    p <- pgamma(q = 1,
                shape = y * alpha,
                rate = alpha * lambda) -
        pgamma(q = 1,
               shape = (y + 1) * alpha,
               rate = alpha * lambda)
    return(p)
}

#' @title Ajuste do Modelo Gamma Count
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @description Estima os parâmetros de um modelo Gamma Count pela
#'     otimização da função de log-verossimilhança definida em
#'     \code{\link{llgcnt}}. A sintaxe assemelha-se com a função
#'     \code{\link[stats]{glm}} (Generalized Linear Models).
#' @param formula Um objeto da classe \code{\link{formula}}. Se
#'     necessária a inclusão de \emph{offset} deve-se indicá-lo como
#'     \code{\link{offset}}.
#' @param data Um objeto de classe \code{data.frame} que contém as
#'     variáveis descritas na \code{formula}.
#' @param start Um vetor com os valores iniciais para os parâmetros do
#'     modelo necessários para o início do procedimento de estimação. Se
#'     \code{NULL} as estimativas de um modelo log-linear Poisson, com
#'     \eqn{\alpha = 0}, são utilizadas como valores iniciais, pois uma
#'     chamada da \code{\link[stats]{glm.fit}} é feita internamente para
#'     obtê-los. O parâmetro \eqn{\alpha} deve ser o primeiro elemento
#'     do vetor. Os restantes devem estar na posição correspondente às
#'     colunas da matriz gerada pelo argumento \code{formula}.
#' @param ... Argumentos opcionais do framework de maximização numérica
#'     \code{\link[bbmle]{mle2}}.
#' @return Um objeto de classe \code{mle2}, retornado da função de
#'     \code{\link[bbmle]{mle2}}, usada para estimação por máxima
#'     verossimilhança de modelos.
#' @import bbmle
#' @examples
#'
#' library(bbmle)
#'
#' str(soja)
#' soja <- soja[-74, ]
#'
#' m0 <- gcnt(nvag ~ bloc + umid * factor(K), data = soja)
#' m1 <- gcnt(nvag ~ bloc + umid + factor(K), data = soja)
#'
#' anova(m0, m1)
#' summary(m1)
#'
#' plot(profile(m1, which = "alpha"))
#' abline(v = 0, lty = 2, col = 2)
#'
#' str(capdesfo)
#'
#' n0 <- gcnt(ncap ~ est * (des + I(des^2)), data = capdesfo)
#' n1 <- gcnt(ncap ~ est + (des + I(des^2)), data = capdesfo)
#'
#' anova(n0, n1)
#' summary(n0)
#'
#' plot(profile(n0, which = "alpha"))
#'
#' @importFrom stats glm.fit model.frame model.matrix model.offset model.response poisson
gcnt <- function(formula, data, start = NULL, ...) {
    frame <- model.frame(formula, data)
    terms <- attr(frame, "terms")
    y <- model.response(frame)
    X <- model.matrix(terms, frame)
    off <- model.offset(frame)
    if (is.null(start)) {
        m0 <- glm.fit(x = X, y = y, offset = off, family = poisson())
        start <- c("alpha" = 0, m0$coefficients)
    }
    bbmle::parnames(llgcnt) <- names(start)
    model <- bbmle::mle2(minuslogl = llgcnt, start = start,
                         data = list(y = y, X = X, offset = off),
                         vecpar = TRUE, ...)
    return(model)
}
