#' @title Avalia\enc{çã}{ca}o da Converg\enc{ê}{e}ncia da Constante Normalizadora
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @description Avalia a converg\enc{ê}{e}ncia da constante de normaliza\enc{çã}{ca}o de um
#'     modelo COM-Poisson definida por: \deqn{Z = \sum
#'     \frac{\lambda^i}{(i!)^\nu}}, em que o parâmetro \eqn{\nu} \enc{é}{e}
#'     tomado como \eqn{\exp{\phi}}.
#' @param model Objeto resultante da fun\enc{çã}{ca}o \code{\link[MRDCr]{cmp}}.
#' @param tol Crit\enc{é}{e}rio de parada do algoritmo, representa o valor
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
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @description Calcula as probabilidades para uma variável aleatória
#'     distribuída conforme modelo COM-Poisson.
#'
#' \deqn{p(y,\lambda,\nu) =
#'     \frac{\lambda^y}{(y!)^\nu Z(\lambda, \nu)}
#' }
#'
#' em que \eqn{Z(\lambda, \nu)} é a constante de normalização definida
#'     por \eqn{\sum_{j=0}^{\infty} \frac{\lambda^j}{(j!)^\nu}}.  Nesta
#'     implementação o número de incrementos considerados para cálculo
#'     dessa constante é definido por \code{sumto}. \eqn{\lambda > 0} e
#'     \eqn{\nu \geq 0} são os parâmetros da distribuição.
#'
#' @param y Valor da variável de contagem.
#' @param lambda Valor do parâmetro \eqn{\lambda} da distribuição
#'     COM-Poisson.
#' @param nu Valor do parâmetro \eqn{\nu} da distribuição COM-Poisson.
#' @param sumto Número de incrementos a serem considerados para a
#'     cálculo da constante normalizadora Z.
#' @examples 
#' dpois(5, lambda = 5)
#' dcmp(5, lambda = 5, nu = 1, sumto = 20)
#'
#' probs <- data.frame(y = 0:30)
#' within(probs, {
#'     py0 <- dpois(y, lambda = 15)
#'     py1 <- dcmp(y, lambda = 15, nu = 1, sumto = 50)
#'     py2 <- dcmp(y, lambda = 915, nu = 2.5, sumto = 50)
#'     py3 <- dcmp(y, lambda = 2.2, nu = 0.3, sumto = 50)
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
#'                CMP(lambda == 15, nu == 1),
#'                CMP(lambda == 915, nu == 2.5),
#'                CMP(lambda == 2.2, nu == 0.3)))
#' })

dcmp <- function(y, lambda, nu, sumto) {
    sapply(y, function(yi) {
        exp(-llcmp(c(log(nu), log(lambda)),
                   y = yi, X = 1, sumto = sumto))
    })
}

#' @name calc_mean_cmp
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @title Calcula o Valor Esperado para a Distribuição
#'     Conway-Maxwell-Poisson
#' @description Função para calcular a média do tipo \eqn{E(Y) = \mu =
#'     \sum y\cdot \Pr(y)} para uma variável aleatória COM-Poisson a
#'     partir dos parâmetros \eqn{\lambda > 0} e \eqn{\nu \geq 0}.
#' @param lambda Valor do parâmetro \eqn{\lambda} da distribuição
#'     COM-Poisson. Quando \eqn{\nu = 1}, o parâmetro \eqn{\lambda =
#'     E(Y)} é a média.
#' @param nu Valor do parâmetro \eqn{\nu} da distribuição COM-Poisson.
#' @param sumto Número de incrementos a serem considerados para a
#'     cálculo da constante normalizadora Z.
#' @param tol Tolerância para interromper a procura pelo valor de
#'     \code{ymax}, valor cuja probabilidade correspondente é inferior a
#'     \code{tol}, para valores os valores de \code{lambda} e
#'     \code{nu} informados.
#' @return Um vetor de tamanho igual ao do maior vetor, \code{lambda} ou
#'     \code{nu} com os valores correspondentes de \eqn{\mu}.

calc_mean_cmp <- function(lambda, nu, sumto, tol = 1e-5) {
    ## Faz com que os parâmetros sejam vetores de mesmo tamanho.
    names(lambda) <- NULL
    names(nu) <- NULL
    pars <- data.frame(lambda = lambda, nu = nu)
    ## Calcula o ymax usando mu + 5 (sqrt(sigma))
    approxmu <- lambda^(1/nu) - (nu - 1)/(2 * nu)
    sigma <- (1/nu) * approxmu
    ymax <- with(pars, ceiling(max(approxmu + 5 * sqrt(sigma))))
    ## Agora verifica se a prob(ymax) é de fato pequena, se não, soma 1.
    lambdamax <- max(pars$lambda)
    numin <- min(pars$nu)
    pmax <- dcmp(y = ymax, lambda = lambdamax, nu = numin, sumto)
    while (pmax > tol) {
        ymax <- ymax + 1
        pmax <- dcmp(y = ymax, lambda = lambdamax, nu = numin, sumto)
    }
    ## Define o vetor onde avaliar a densidade COM-Poisson.
    y <- 1:ymax
    estmean <- mapply(lambda = pars$lambda,
                      nu = pars$nu,
                      MoreArgs = list(y = y, sumto = sumto),
                      FUN = function(lambda, nu, y, sumto) {
                          py <- dcmp(y, lambda, nu, sumto)
                          sum(y * py)
                      },
                      SIMPLIFY = TRUE)
    names(estmean) <- NULL
    return(estmean)
}

#' @name calc_var_cmp
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @title Calcula o Valor da Variância para a Distribuição
#'     Conway-Maxwell-Poisson
#' @description Função para calcular a variância do tipo \eqn{V(Y) =
#'     E(Y^2) - E^2(Y) = \sum y^2\cdot \Pr(y) - \left ( \sum y\cdot
#'     \Pr(y) \right )^2} para uma variável aleatória COM-Poisson a
#'     partir dos parâmetros \eqn{\lambda > 0} e \eqn{\nu \geq 0}.
#' @param lambda Valor do parâmetro \eqn{\lambda} da distribuição
#'     COM-Poisson. Quando \eqn{\nu = 1}, o parâmetro \eqn{\lambda =
#'     E(Y)} é a média.
#' @param nu Valor do parâmetro \eqn{\nu} da distribuição COM-Poisson.
#' @param sumto Número de incrementos a serem considerados para a
#'     cálculo da constante normalizadora Z.
#' @param tol Tolerância para interromper a procura pelo valor de
#'     \code{ymax}, valor cuja probabilidade correspondente é inferior a
#'     \code{tol}, para valores os valores de \code{lambda} e \code{nu}
#'     informados.
#' @return Um vetor de tamanho igual ao do maior vetor, \code{lambda} ou
#'     \code{nu} com os valores correspondentes de \eqn{\mu}.

calc_var_cmp <- function(lambda, nu, sumto, tol = 1e-5) {
    # Faz com que os parâmetros sejam vetores de mesmo tamanho.
    names(lambda) <- NULL
    names(nu) <- NULL
    pars <- data.frame(lambda = lambda, nu = nu)
    ## Calcula o ymax usando mu + 5 (sqrt(sigma))
    approxmu <- lambda^(1/nu) - (nu - 1)/(2 * nu)
    sigma <- (1/nu) * approxmu
    ymax <- with(pars, ceiling(max(approxmu + 5 * sqrt(sigma))))
    # Agora verifica se a prob(ymax) é de fato pequena, se não, soma 1.
    lambdamax <- max(pars$lambda)
    numin <- min(pars$nu)
    pmax <- dcmp(y = ymax, lambda = lambdamax, nu = numin, sumto)
    while (pmax > tol) {
        ymax <- ymax + 1
        pmax <- dcmp(y = ymax, lambda = lambdamax, nu = numin, sumto)
    }
    # Define o vetor onde avaliar a densidade COM-Poisson.
    y <- 1:ymax
    esty2 <- mapply(lambda = pars$lambda,
                      nu = pars$nu,
                      MoreArgs = list(y = y, sumto = sumto),
                      FUN = function(lambda, nu, y, sumto) {
                          py <- dcmp(y, lambda, nu, sumto)
                          c(sum(y * py)^2, sum(y^2 * py))
                      },
                      SIMPLIFY = TRUE)
    estvar <- diff(esty2)
    names(estvar) <- NULL
    return(estvar)
}

#' @title Ajuste do Modelo Conway-Maxwell-Poisson
#' @author Eduardo E. R. Junior, \email{edujrrib@gmail.com}
#' @export
#' @description Estima os parâmetros de um modelo COM-Poisson pela
#'     otimização da função de log-verossimilhança definida em
#'     \code{\link{llcmp}}. A sintaxe assemelha-se com a função
#'     \code{\link{glm}} (Generalized Linear Models).
#' @param formula Um objeto da classe \code{\link{formula}}.
#' @param data Um objeto de classe \code{data.frame}, que contém as
#'     variáveis descritas na \code{formula}.
#' @param start Um vetor nomeado com os valores iniciais para os
#'     parâmetros do modelo necessários para o início do procedimento de
#'     estimação. Se \code{NULL} as estimativas de um modelo log-linear
#'     Poisson, com \eqn{\phi = 0}, são utilizadas como valores
#'     iniciais, pois uma chamada da \code{\link[stats]{glm.fit}} é
#'     feita internamente para obtê-los. O parâmetro \eqn{\phi} deve ser
#'     o primeiro elemento do vetor. Os restantes devem estar na
#'     correspondente às colunas da matriz gerada pelo argumento
#'     \code{formula}.
#' @param sumto Número de incrementos a serem considerados para a soma
#'     daa constantea normalizadoraa. Como padrão, \code{NULL} o número
#'     de incrementos é o valor inteiro de \eqn{(\max y)^1.2}, porém
#'     ressalta-se que este valor padrão não é o ideal. Uma avaliação da
#'     escolha desse argumento, pós ajuste pode ser realizada via
#'     \code{\link[MRDCr]{convergencez}}.
#' @param ... Argumentos opcionais do framework de maximização numérica
#'     \code{\link[bbmle]{mle2}}.
#' @return Um objeto de classe \code{mle2}, retornado da função de
#'     \code{\link[bbmle]{mle2}}, usada para ajuste de modelos por
#'     máxima verossimilhança.
#' @import bbmle
#' @importFrom stats glm.fit model.frame model.matrix model.offset
#'     model.response poisson
#' @examples
#' str(capdesfo)
#' m0 <- cmp(ncap ~ est + (des + I(des^2)), data = capdesfo, sumto = 40)
#' m1 <- cmp(ncap ~ est * (des + I(des^2)), data = capdesfo, sumto = 40)
#' 
#' convergencez(m0)
#' convergencez(m1)
#' 
#' library(bbmle)
#' anova(m0, m1)
#' summary(m1)
#'
#' \dontrun{
#' plot(profile(m1, which = "phi"))
#' }
cmp <- function(formula, data, start = NULL, sumto = NULL, ...) {
    ##-------------------------------------------
    ## Constrói as matrizes do modelo
    frame <- model.frame(formula, data)
    terms <- attr(frame, "terms")
    y <- model.response(frame)
    X <- model.matrix(terms, frame)
    if(!is.null(model.offset(frame))) {
        stop("Este modelo ainda nao suporta offset")
    }
    if (is.null(sumto)) sumto <- ceiling(max(y)^1.2)
    ##-------------------------------------------
    ## Define os chutes iniciais
    if (is.null(start)) {
        m0 <- glm.fit(x = X, y = y, family = poisson())
        start <- c("phi" = 0, m0$coefficients)
    }
    ##-------------------------------------------
    ## Nomeia os parâmetros da função para métodos bbmle
    bbmle::parnames(llcmp) <- names(start)
    model <- bbmle::mle2(llcmp, start = start,
                         data = list(y = y, X = X,
                                     sumto = sumto),
                         vecpar = TRUE, ...)
    return(model)
}
