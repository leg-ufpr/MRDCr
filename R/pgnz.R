#' @title Log-Verossimilhan\enc{ç}{c}a do Modelo Poisson Generalizada
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#' @export
#' @description Calcula a log-verossimilhança de um modelo de regressão
#'     para a média com distribuição Poisson Generalizada para a
#'     resposta de contagem (y).
#' @details A função de log-verossimilhança da Poisson Generalizada, na
#'     parametrização de modelo de regressão é:
#'
#' \deqn{\ell(\lambda,\alpha,y) =
#'   y (\log(\lambda) - \log(1 + \alpha\lambda)) +
#'   (y - 1) \log(1 + \alpha y) -
#'   \lambda\left(\frac{1 + \alpha y}{1 + \alpha\lambda}\right) -
#'   \log(y!), }
#'
#' em que \eqn{\alpha} é o parâmetro de dispersão e \eqn{\lambda > 0} é
#'     a média \eqn{E(Y) = \lambda} e \eqn{y = 0,1,ldots} é vetor
#'     observado da variável de contagem. Nessa parametrização,
#'     \eqn{V(Y) = \lambda (1 + \alpha\lambda)^2}. A Poisson
#'     Generalizada em a Poisson como caso particular quando \eqn{\alpha
#'     = 0}.
#'
#' Para o modelo de regressão, um preditor linear é ligado à média pela
#'     função de ligação log, \eqn{\log(\lambda) = X\beta}, tal como é
#'     para o modelo Poisson com link log.
#'
#' O espaço paramétrico de \eqn{\alpha} não limitado para o lado direito
#'     do zero (\eqn{\alpha} positivo) mas para o lado esquerdo
#'     (\eqn{\alpha} negativo) o limite inferior é dependente do
#'     parâmetro \eqn{\lambda} e dos valores observados de
#'     \eqn{y}. Valores não finitos podem ser retornados durante a
#'     estimação quando \eqn{1 + \alpha\lambda} ou \eqn{1 + \alpha y}
#'     não forem maiores que zero.
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
#' @return O negativo da log-verossimilhança do modelo Poisson
#'     Generalizado para os parâmetros e dados informados.
#' @seealso \code{\link[bbmle]{mle2}}.
#' @examples
#'
#' set.seed(123)
#' y <- rpois(10, lambda = 5)
#'
#' # Log-verossimilhança pela Poisson.
#' sum(dpois(y, lambda = 5, log = TRUE))
#'
#' # Log-verossimilhança pela PGNZ usando alpha = 0
#' llpgnz(params = c(0, log(5)), y = y, X = cbind(y * 0 + 1))
#'
#' set.seed(121)
#' y <- rpois(100, lambda = exp(1))
#' X <- cbind(0 * y + 1)
#'
#' grid <- expand.grid(alpha = seq(-0.1, 0.4, by = 0.01),
#'                     lambda = seq(0.1, 2.1, by = 0.025))
#'
#' grid$ll <- apply(grid, MARGIN = 1,
#'                  FUN = function(vec) {
#'                      llpgnz(params = vec, y = y, X = X, offset = NULL)
#'                  })
#'
#' library(latticeExtra)
#'
#' levelplot(ll ~ alpha + lambda, data = grid) +
#'     layer(panel.abline(v = 0, h = 1, lty = 2))
#'
llpgnz <- function(params, y, X, offset = NULL) {
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
    alpha <- params[1]
    lambda <- offset * exp(X %*% params[-1])
    z <- 1 + alpha * lambda
    w <- 1 + alpha * y
    ll <- y * (log(lambda) - log(z)) +
        (y - 1) * log(w) -
        lambda * (w/z) -
        lfactorial(y)
    # Negativo da log-likelihood.
    -sum(ll)
}

#' @title Probabilidades do Modelo Poisson Generalizado
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#' @export
#' @description Calcula as probabilidades para uma variável aleatória
#'     com distribuição Poisson Generalizada na parametrização
#'     \eqn{\lambda-\alpha}:
#'
#' \deqn{p(y,\lambda,\alpha) =
#'   \left(\frac{\lambda}{1+\alpha\lambda}\right)^{y}
#'   \,\frac{(1+\alpha y)^{y-1}}{y!}
#'   \exp\left\{-\lambda\left(
#'   \frac{1+\alpha y}{1+\alpha\lambda}\right)\right\},
#' }
#'
#' em que \eqn{\lambda > 0} é a média da variável aleatória e
#'     \eqn{\alpha} é o parâmetro de dispersão, sendo que \eqn{V(Y) =
#'     \lambda (1 + \alpha\lambda)^2}. O espaço paramétrico de
#'     \eqn{\alpha} depende de \eqn{\lambda} e \eqn{y} pois
#'     \eqn{1+\alpha\lambda > 0} e \eqn{1+\alpha y > 0}.
#' @param y Valor da variável de contagem.
#' @param lambda Valor do parâmetro \eqn{\lambda} que é a média da
#'     distribuição .
#' @param alpha Valor do parâmetro \eqn{\alpha} que é o parâmetro de
#'     dispersão.
#' @return Retorna uma probabilidade, ou seja \eqn{\Pr(Y = y | \lambda,
#'     \alpha) = p(y, \lambda, \alpha)}.
#' @examples
#'
#' dpois(5, lambda = 5)
#' dpgnz(5, lambda = 5, alpha = 0)
#'
#' probs <- data.frame(y = 0:30)
#' within(probs, {
#'     py0 <- dpois(y, lambda = 15)
#'     py1 <- dpgnz(y, lambda = 15, alpha = 0)
#'     py2 <- dpgnz(y, lambda = 15, alpha = 1/30)
#'     py3 <- dpgnz(y, lambda = 15, alpha = -1/30)
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
#'                PG(lambda == 15, alpha == 0),
#'                PG(lambda == 15, alpha == 1/30),
#'                PG(lambda == 15, alpha == -1/30)))
#' })
#'
dpgnz <- function(y, lambda, alpha) {
    k <- lfactorial(y)
    w <- 1 + alpha * y
    z <- 1 + alpha * lambda
    m <- alpha > pmax(-1/y, -1/lambda)
    fy <- y * (log(lambda) - log(z)) +
        (y - 1) * log(w) - lambda * (w/z) - k
    fy[!m] <- 0
    return(m * exp(fy))
}

#' @title Ajuste do Modelo Poisson Generalizado
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#' @export
#' @description Estima os parâmetros de um modelo Poisson Generalizado
#'     pela otimização da função de log-verossimilhança definida em
#'     \code{\link{llpgnz}}. A sintaxe assemelha-se com a função
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
#'     do vetor. Os restantes devem estar na correspondente às colunas
#'     da matriz gerada pelo argumento \code{formula}.
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
#' m0 <- pgnz(nvag ~ bloc + umid * factor(K), data = soja)
#' m1 <- pgnz(nvag ~ bloc + umid + factor(K), data = soja)
#'
#' anova(m0, m1)
#' summary(m1)
#'
#' plot(profile(m1, which = "alpha"))
#' abline(v = 0, lty = 2, col = 2)
#'
#' str(capdesfo)
#'
#' n0 <- pgnz(ncap ~ est * (des + I(des^2)), data = capdesfo)
#' n1 <- pgnz(ncap ~ est + (des + I(des^2)), data = capdesfo)
#'
#' anova(n0, n1)
#' summary(n0)
#'
#' plot(profile(n0, which = "alpha"))
#'
#' @importFrom stats glm.fit model.frame model.matrix model.offset model.response poisson
pgnz <- function(formula, data, start = NULL, ...) {
    if (!requireNamespace("bbmle", quietly = TRUE)){
        stop(paste("`bbmle` \u00e9 necess\u00e1rio para",
                   "essa fun\u00e7\u00e3o. Por favor, instale-o."),
             call. = FALSE)
    }
    frame <- model.frame(formula, data)
    terms <- attr(frame, "terms")
    y <- model.response(frame)
    X <- model.matrix(terms, frame)
    off <- model.offset(frame)
    if (is.null(start)) {
        m0 <- glm.fit(x = X, y = y, offset = off, family = poisson())
        start <- c("alpha" = 0, m0$coefficients)
    }
    off <- if (is.null(off)) { NULL } else { exp(off) }
    bbmle::parnames(llpgnz) <- names(start)
    model <- bbmle::mle2(minuslogl = llpgnz, start = start,
                         data = list(y = y, X = X, offset = off),
                         vecpar = TRUE, ...)
    return(model)
}
