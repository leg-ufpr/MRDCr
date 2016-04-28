#' @name apc
#' @export
#' @author Walmes Zeviani baseado na lista de discussão R-help.
#' @title Gera a Matriz de Constrates de Tukey
#'
#' @description Essa função retorna os contrastes de Tukeu entre médias
#'     a partir da matriz que define as funções lineares dos
#'     coeficientes do modelo para estimar a média de mínimos
#'     quadrados. Essas matrizes podem ser utilizadas para fazer
#'     contrastes entre médias por meio da função
#'     \code{multcomp::glht()}.
#'
#' @param lfm Uma matriz de dimensões \eqn{k\times p} onde cada linha
#'     tem os coeficientes correspondentes para estimar uma média. Essas
#'     matrizes são facilmente obtidas com usando a função
#'     \code{doBy::LSmatrix()}.
#'
#' @param lev Um vetor com os nomes dos níveis do fator para o qual
#'     correspondem as linhas da matriz usada em \code{lfm}. Portanto, o
#'     número de elementos do vetor deve ser igual ao número de linhas
#'     da matriz. O valor default é \code{NULL} e então é usado o
#'     \code{rownames()} da matriz. Se \code{rownames()} for
#'     \code{NULL}, uma sequência de números começando em 1 é utilizada
#'     para representar cada linha.
#'
#' @return Uma matriz \eqn{r\times p} cujas linhas estimam o contraste
#'     entre cada possível par de médias, portanto \eqn{r} is
#'     \eqn{{k}\choose{2}}.
#'
#' @seealso \code{\link[doBy]{LSmatrix}}.
#' @import doBy multcomp
#' @examples
#'
#' X <- diag(3)
#' rownames(X)
#' apc(X)
#'
#' rownames(X) <- LETTERS[nrow(X):1]
#' apc(X)
#'
#' apc(X, lev = LETTERS[1:nrow(X)])
#'
#' #-----------------------------------------
#' # Usando para estimar as médias e contrates.
#'
#' xtabs(~tension + wool, data = warpbreaks)
#'
#' warpbreaks <- transform(warpbreaks,
#'                         trt = interaction(tension, wool))
#'
#' m0 <- lm(log(breaks) ~ trt, data = warpbreaks)
#' anova(m0)
#'
#' library(doBy)
#'
#' L <- LSmatrix(m0, effect = "trt")
#' L
#'
#' K <- apc(L)
#' K
#'
#' library(multcomp)
#'
#' glht(m0, linfct = L)
#' glht(m0, linfct = K)
#'
#' aggregate(cbind(log(breaks)) ~ trt, data = warpbreaks, FUN = mean)
#'
apc <- function(lfm, lev = NULL) {
    nlev <- nrow(lfm)
    rn <- rownames(lfm)
    a <- attr(lfm, "grid")
    if (is.null(lev)) {
        if (!is.null(a)) {
            lev <- apply(a, MARGIN = 1, FUN = paste, collapse = ":")
        } else if (!is.null(rn)) {
            lev <- rn
        } else {
            lev <- as.character(1:nlev)
        }
    }
    cbn <- combn(x = seq_along(lev), m = 2)
    M <- lfm[cbn[1, ], ] - lfm[cbn[2, ], ]
    if (is.vector(M)) {
        dim(M) <- c(1, length(M))
    }
    rownames(M) <- paste(lev[cbn[1, ]], lev[cbn[2, ]], sep = "-")
    return(M)
}
