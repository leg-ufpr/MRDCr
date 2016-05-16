#' @title Avaliação da Convergência da Constante Normalizadora
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @description Avalia a convergência da constante de normalização de um
#'     modelo COM-Poisson definida por: \deqn{Z = \sum
#'     \frac{\lambda^i}{(i!)^\nu}}, em que o parâmetro \eqn{\nu} é
#'     tomado como \eqn{\exp{\phi}}.
#' @param model Objeto resultante da função \code{\link[MRDCr]{cmp}}.
#' @param tol Critério de parada do algoritmo, representa o valor
#'     tolerado para a diferença de \eqn{\frac{\lambda^i}{(i!)^\nu} -
#'     0}, pois no limite \eqn{i \rightarrow \infty} o incremente
#'     \eqn{\frac{\lambda^i}{(i!)^\nu}} tende a 0.
#' @param incremento Número de incrementos da soma a serem considerados
#'     a cada iteração. Padrão definido como 10, ou seja, a cada
#'     iteração 10 incrementos são calculados.
#' @param maxit Número máximo de iterações a serem realizadas pelo
#'     algoritmo. Se este número for atingido e o critério de tolerância
#'     não for atendido, uma mensagem de aviso será exibida.
#' @param plot Argumento lógico. Se \code{TRUE} (padrão) os gráficos dos
#'     incrementos daa constantes, calculadas para cada observação são
#'     exibidos.
#' @return Uma lista com os incrementos das constantes Z,
#'     \eqn{Z(\lambda, \phi)} da distribuição COM-Poisson, calculados
#'     para cada observação.
#' @examples
#'
#' m1 <- cmp(ncap ~ est * (des + I(des^2)), data = capdesfo)
#' tablez <- convergencez(m1)
#' str(tablez)
#' 
#' m2 <- cmp(ncap ~ dexp + I(dexp^2), data = capmosca)
#' tablez <- convergencez(m2)
#' str(tablez)
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices extendrange
convergencez <- function(model, tol = 1e-4, incremento = 10,
                         maxit = 150, plot = TRUE) {
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
    stcalc <- max(sapply(zs, length))
    ##-------------------------------------------
    n <- length(zs)
    id <- c(); ii <- c()
    for (i in 1:n) {
        l <- length(zs[[i]])
        ii <- c(ii, 1:l)
        id <- c(id, rep(i, times = l))
    }
    da <- data.frame(zs = unlist(zs), id = id, ii = ii)
    ##-------------------------------------------
    if (plot) {
        ylab = expression(frac(lambda^j, "(j!)"^nu))
        sumtos <- c(sumto, stcalc)
        print(
            xyplot(zs ~ ii, groups = id, 
                   data = da, type = "l",
                   ylab = list(ylab, rot = 0),
                   xlab = "j",
                   xlim = extendrange(c(1, max(sumtos))),
                   panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.abline(v = sumtos, h = 0)
                       panel.text(
                           x = sumtos*0.95,
                           y = max(y)*c(0.9, 0.8),
                           labels =
                               paste("sumto\n",
                                     c("considerado", "calculado")))
                   })
        )
    }
    invisible(da)
}

#' @title Log-Verossimilhança do Modelo Conway-Maxwell-Poisson
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @description Calcula a log-verossimilhança de um modelo de regressão
#'     para o parâmetro \eqn{\lambda} considerando as respostas de
#'     contagem (y), condicionadas as suas covariáveis (X), distribuídas
#'     conforme modelo COM-Poisson.
#' @details A função de log-verossimilhança da COM-Poisson, na
#'     parametrização de modelo de regresssão é:
#'
#' \deqn{\ell(\beta, \nu, y) =
#'     \sum_{i=1}^{n} y_i \log(\lambda_i) - \nu \sum_{i=1}^{n}\log(y!)
#'     - \sum_{i=1}^{n} \log(Z(\lambda_i, \nu))}
#'
#' em que (i) \eqn{\lambda_i = \exp(X_i \beta)}, no modelo de regressão
#'     COM-Poisson um preditor linear é ligado à \eqn{\lambda} por meio
#'     da função de ligação log. Note que não estamos modelando
#'     diretamente a média, assim as estimativas dos parâmetros
#'     \eqn{\beta} não tem a mesma interpretação dos modelos Poisson,
#'     por exemplo. Contudo, os sinais desses parâmetros indicam efeitos
#'     de acréscimo ou descréscimo nas contagens médias.
#' (ii) \eqn{\nu} é o parâmetro de dispersão que indica equi, sub ou
#'     superdispersão das contagens y. Sendo \eqn{nu = 1} o caso de
#'     equidispersão, \eqn{0 \leq \nu < 1} superdispersão e \eqn{\nu >
#'     1} subdispersão. Vale ressaltar que a variância \eqn{V(Y)} não
#'     tem expressão fechada e não é definada unicamente por \eqn{\nu}.
#' (iii) \eqn{Z(\lambda_i, \nu)} é a constante de normalização definida
#'     por \deqn{\sum_{j=0}^{\infty} \frac{\lambda_i^j}{(j!)^\nu}}. Note
#'     que são cálculadas n constantes Z. Nesta implementação o número
#'     de incrementos considerados para cálculo dessas constantes é
#'     definido por \code{sumto}, o mesmo número de incrementos é
#'     considerado para o cálculo de todas as contantes. Uma verificação
#'     pós ajuste da escolha de \code{sumto} pode ser realizada a partir
#'     de \code{\link[MRDCr]{convergencez}}.
#'
#' Nesta parametrização o modelo COM-Poisson tem como casos particulares
#'     os modelos Poisson quando \eqn{\nu = 1}, Bernoulli quando
#'     \eqn{\nu \rightarrow \infty} (ou o modelo logístico considerando
#'     modelos de regressão) e Geométrico quando \eqn{\nu = 0} e
#'     \eqn{\lambda < 1}.
#'
#' Para que não seja necessário restringir o algoritmo de maximização da
#'     log-verossimilhança, a função foi implementada reparametrizando o
#'     parâmetro \eqn{\nu} para \eqn{\log(\phi)}. Assim o parâmetro
#'     estimado será \eqn{\phi} que tem suporte nos reais, assim como o
#'     vetor \eqn{\beta}.
#' @param params Um vetor de parâmetros do modelo COM-Poisson. O
#'     primeiro elemento desse vetor deve ser o parâmetro de dispersão
#'     do modelo, \eqn{\phi}, os restantes são os parâmetros
#'     \eqn{\beta}'s associados ao preditor linear em \eqn{\lambda}.
#' @param y Um vetor com variável dependente do modelo, resposta do tipo
#'     contagem.
#' @param X A matriz de delineamento correspondente ao modelo linear
#'     ligado à \eqn{\lambda} pela função de ligação log. A matriz do
#'     modelo pode ser construída com a função
#'     \code{\link[stats]{model.matrix}}.
#' @param sumto Número de incrementos a serem considerados para a
#'     cálculo das constantes normalizadoras. Como padrão, o número de
#'     incrementos é o valor inteiro de \eqn{(\max y)^1.5}, porém esse
#'     valor padrão não é ótimo. Uma avaliação da escolha desse
#'     argumento, pós ajuste pode ser realizada via
#'     \code{\link[MRDCr]{convergencez}}.
#' @return O negativo da log-verossimilhança do modelo
#'     Conway-Maxwell-Poisson com os parâmetros e dados informados.
#' @seealso \code{\link[bbmle]{mle2}}

llcmp <- function(params, y, X, sumto = ceiling(max(y)^1.2)){
    ##-------------------------------------------
    betas <- params[-1]
    phi <- params[1]
    nu <- exp(phi)
    ##-------------------------------------------
    Xb <- X %*% betas
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
