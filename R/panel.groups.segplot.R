#' @name panel.groups.segplot
#' @export
#' @author Walmes Zeviani
#' @title Painel para Fazer Intervalos para Grupos no \code{segplot()}
#'
#' @description Essa função permite fazer intervalos de confiança (ou
#'     com barras de erro) em gráficos da \code{latticeExtra::segplot()}
#'     para grupos (\code{groups =}) de tal forma que eles não fiquem
#'     sobrepostos.
#'
#' @param x,y,z,centers,data,subscripts,... Argumentos passados para a
#'     \code{\link[latticeExtra]{segplot}}.
#'
#' @param groups A variável (\code{factor}) de agrupamento, de mesmo
#'     comprimento de \code{lwr} e \code{upr}.
#'
#' @param gap Escalar que representa a distância entre os segmentos. O
#'     valor default é 0,1. Como um fator com \eqn{k} níveis é
#'     representado pelos números inteiros \eqn{1, 2, \ldots, k}, então
#'     \eqn{0 \leq \textrm{gap} < 1/k}. Se for usado um valor negativo,
#'     os intervalos serão apresentados em ordem inversa.
#'
#' @return A função é passada para o argumento \code{panel} e retorna
#'     elementos necessários para a \code{\link[latticeExtra]{segplot}}.
#'
#' @seealso \code{\link[latticeExtra]{segplot}}.
#' @import latticeExtra
#' @examples
#'
#' library(latticeExtra)
#'
#' m0 <- lm(log(breaks) ~ wool * tension, data = warpbreaks)
#'
#' pred <- with(warpbreaks, expand.grid(KEEP.OUT.ATTRS = TRUE,
#'                                      wool = levels(wool),
#'                                      tension = levels(tension)))
#'
#' pred <- cbind(pred,
#'               predict(m0, newdata = pred, interval = "confidence"))
#' str(pred)
#'
#' segplot(wool ~ lwr + upr, centers = fit, data = pred,
#'         draw = FALSE, horizontal = FALSE)
#'
#' segplot(wool ~ lwr + upr, centers = fit, data = pred,
#'         draw = FALSE, horizontal = FALSE,
#'         groups = tension, gap = NULL,
#'         panel = panel.groups.segplot)
#'
panel.groups.segplot <- function(x, y, z, centers,
                                 groups, gap = NULL,
                                 data, subscripts, ...) {
    if (!missing(data)) {
        data <- eval(data, envir = parent.frame())
        groups <- data[, deparse(substitute(groups))]
    }
    stopifnot(is.factor(groups))
    stopifnot(length(groups) == length(z))
    if (is.null(gap)) {
        gap <- 0.5/nlevels(groups)
    }
    d <- 2 * ((as.numeric(groups) - 1)/(nlevels(groups) - 1)) - 1
    z <- as.numeric(z) + gap * d
    panel.segplot(x, y, z, centers = centers,
                  subscripts = subscripts, ...)
}
