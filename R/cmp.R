#' @title Avaliação da Convergência da Constante Normalizadora
#' @description Avalia a convergência da constante de normalização de
#'     um modelo COM-Poisson definida por: \deqn{Z = \sum
#'     \frac{\lambda^i}{(i!)^\nu}}, em que o parâmetro \eqn{\nu} é
#'     tomado como \eqn{\exp{\phi}}.
#' @param model Objeto resultante da função \code{\link[MRDCr]{cmp}}.
#' @param tol Critério de parada do algoritmo, representa o valor
#'     tolerado para a diferença do valor de \eqn{Z(\lambda, \phi)}
#'     entre duas iterações. O valor padrão é 1e-4
#' @param incremento Número de incrementos da soma a serem considerados
#'     a cada iteração. Padrão definido como 10, ou seja, a cada
#'     iteração 10 passos incrementos são somados a Z.
#' @param maxit Número máximo de iterações a serem realizadas pelo
#'     algoritmo. Se este número for atingido e o critério de tolerância
#'     não for atendido, uma mensagem de aviso será exibida.
#' @param plot Argumento lógico. Se \code{TRUE} (padrão) os gráficos dos
#'     incrementos da constante são exibidos.
#' @return Uma lista com os incrementos das constantes Z,
#'     \eqn{Z(\lambda, \phi)} da distribuição COM-Poisson, calculados
#'     para cada observação.
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export

convergencez <- function(model, tol = 1e-4, incremento = 10,
                         maxit = 50, plot = TRUE) {
    ##-------------------------------------------
    ## Calcula Z para um c(lambda, phi)
    calcula <- function(loglambda, phi) {
        nu <- exp(phi)
        zg <- vector("list", maxit)
        t <- incremento
        i <- 1:t
        j <- 1
        zg[[j]] <- exp(i * loglambda - nu * lfactorial(i))
        dif <- abs(zg[[j]][t-1] - zg[[j]][t])
        while (dif > tol && is.finite(dif) && j <= maxit) {
            i <- (i[t] + 1):(i[t] + t)
            j <- j + 1
            zg[[j]] <- exp(i * loglambda - nu * lfactorial(i))
            dif <- abs(zg[[j]][t-1] - zg[[j]][t])
        }
        if (j == maxit && dif > tol || !is.finite(dif)) {
            print(c(loglambda, nu))
            warning("Soma n\\u00e3o convergiu")
        }
        return(z = unlist(zg))
    }
    ##-------------------------------------------
    X <- model@data$X
    y <- model@data$y
    sumto <- model@data$sumto
    betas <- model@coef[-1]
    phi <- model@coef[1]
    loglambdas <- X %*% betas
    zs <- sapply(loglambdas, calcula, phi, simplify = FALSE)
    ##-------------------------------------------
    if (plot) {
        mx <- max(sapply(zs, max))
        lx <- max(sapply(zs, length))
        plot(zs[[1]], type = "l", xlim = c(0, lx), ylim = c(0, mx))
        abline(v = sumto)
        for (i in 2:length(zs)) lines(zs[[i]], type = "l")
    }
    invisible(zs)
}

#' @title Log-Verossimilhança do Modelo Conway-Maxwell-Poisson
#' @description Calcula a log-verossimilhança de um modelo COM-Poisson
#'     considerando os dados e as estimativas dos parâmetros informadas.
#' @details A função de log-verossimilhança toma a forma: \deqn{-Z - y *
#'     \lambda - \nu \log{y!}}, onde \eqn{Z = \sum
#'     \frac{\lambda^i}{(i!)^\nu}} e \eqn{\nu = \exp{\phi}}.
#' @param params Um vetor de parâmetros do modelo COM-Poisson. É
#'     necessário que seja informado como primeiro elemento do vetor, o
#'     valor de \eqn{\phi}. Os demais elementos são os \eqn{\beta}'s do
#'     preditor linear \eqn{\lambda}.
#' @param y Um vetor de contagens, considerado como variável resposta.
#' @param X A matriz de delineamento do modelo.
#' @param offset Um vetor de valores a serem adicionados ao preditor
#'     linear.
#' @param sumto Número de incrementos a serem considerados para a soma
#'     da constante normalizadora. Como padrão, o número de incrementos
#'     é o valor inteiro de \eqn{(\max y)^1.2}.
#' @return O valor da log-verossimilhança do modelo
#'     Conway-Maxwell-Poisson com os parâmetros e dados informados.
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @seealso \code{\link[bbmle]{mle2}}
#' @export

llcmp <- function(params, y, X, offset = NULL,
                  sumto = ceiling(max(y)^1.5)){
    ##-------------------------------------------
    betas <- params[-1]
    phi <- params[1]
    nu <- exp(phi)
    ##-------------------------------------------
    if (is.null(offset)) offset <- 0
    Xb <- X %*% betas + offset
    ##-------------------------------------------
    ## Obtendo a constante normatizadora Z.
    i <- 0:sumto
    zs <- sapply(Xb, function(loglambda)
        sum(exp(i * loglambda - nu * lfactorial(i))))
    Z <- sum(log(zs))
    ##-------------------------------------------
    ll <- sum(y * Xb - nu * lfactorial(y)) - Z
    return(-ll)
}

#' @title Probabilidades do Modelo Conway-Maxwell-Poisson
#' @description Calcula as probabilidades conforme modelo COM-Poisson
#' @param y Valor da contagem
#' @param loglambda Logaritmo neperiano do parâmetro \eqn{\lambda} da
#'     distribuição
#' @param phi Valor do parâmetro \eqn{\phi}, definido como
#'     \eqn{\log(\nu)} na distribuição de probabilidades COM-Poisson
#' @param sumto Número de incrementos a serem considerados para a soma
#'     da constante normalizadora Z.
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @examples 
#' dpois(5, 5)
#' dcmp(5, log(5), 0, sumto = 20)
#' 
#' x <- 0:30
#' px1 <- dpois(x, 15)
#' px2 <- dcmp(x, log(915), log(2.5), sumto = 50)
#' px3 <- dcmp(x, log(2.2), log(0.3), sumto = 50)
#' 
#' plot(y = px2, x = x, type = "h", lwd = 2)
#' lines(y = px1, x = x - 0.2, type = "h", lwd = 2, col = 4)
#' lines(y = px3, x = x + 0.2, type = "h", lwd = 2, col = 2)
#' @export

dcmp <- function(y, loglambda, phi, sumto) {
    sapply(y, function(yi) {
        exp(-llcmp(c(phi, loglambda), y = yi, X = 1, sumto = sumto))
    })
}

#' @title Ajuste do Modelo Conway-Maxwell-Poisson
#' @description Estima os parâmetros de um modelo COM-Poisson sob a
#'     otimização da função de log-verossimilhança. A sintaxe
#'     assemelha-se com a função \code{\link{glm}} (Generalized Linear
#'     Models).
#' @param formula Um objeto da classe \code{\link{formula}}. Se
#'     necessária a inclusão de \emph{offset} deve-se indicá-lo como
#'     \code{\link{offset}}.
#' @param data Um objeto de classe \code{data.frame}, cujo contém as
#'     variáveis descritas na \code{formula}.
#' @param start Um vetor de parâmetros do modelo tomados como valores
#'     iniciais para o algoritmo iterativo. Se \code{NULL} os parâmetros
#'     de um modelo log-linear Poisson e \eqn{\phi = 0} são tomados como
#'     valores iniciais para o preditor e para o parâmetro de dispersão.
#' @param sumto Número de incrementos a serem considerados para a soma
#'     da constante normalizadora. Como padrão, \code{NULL} o número de
#'     incrementos é o valor inteiro de \eqn{(\max y)^1.2}.
#' @param ... Argumentos opcionais do framework de maximização numérica
#'     \code{\link[bbmle]{mle2}}.
#' @return Um objeto de classe \code{mle2}, retornado da função de
#'     \code{\link[bbmle]{mle2}}, usada para ajuste de modelos por
#'     máxima verossimilhança.
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @import bbmle
#' @export

cmp <- function(formula, data, start = NULL, sumto = NULL, ...) {
    ##-------------------------------------------
    ## Constrói as matrizes do modelo
    frame <- model.frame(formula, data)
    terms <- attr(frame, "terms")
    y <- model.response(frame)
    X <- model.matrix(terms, frame)
    off <- model.offset(frame)
    if (is.null(sumto)) sumto <- ceiling(max(y)^1.5)
    ##-------------------------------------------
    ## Define os chutes iniciais
    if (is.null(start)) {
        m0 <- glm.fit(x = X, y = y, offset = off, family = poisson())
        start <- c("phi" = 0, m0$coefficients)
    }
    ##-------------------------------------------
    ## Nomeia os parâmetros da função para métodos bbmle
    bbmle::parnames(llcmp) <- names(start)
    model <- bbmle::mle2(llcmp, start = start,
                         data = list(y = y, X = X, offset = off,
                                     sumto = sumto),
                         vecpar = TRUE, ...)
    return(model)
}
