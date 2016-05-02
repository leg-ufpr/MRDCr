#' @name panel.beeswarm
#' @aliases panel.beeswarm
#' @export
#' @author Walmes Zeviani
#' @title Função para Gráfico de Dispersão BeesWarm com a Lattice
#'
#' @description Essa função permite a construção de gráficos de
#'     dispersão unidimensionais similares a função
#'     \code{\link{stripchart}}. Porém com pontos de mesmas coordenadas
#'     exibidos lado a lado e não sobrepostos.
#'
#' @param x,y,subscripts,... Argumentos passados para a
#'     função \code{\link[lattice]{xyplot}}.
#'
#' @param r Valor para espaçamento entre os pontos de mesmas
#'     coordenadas.
#'
#' @return Painel \code{xyplot} padrão, com as coordenadas \code{x}
#'     devidamente corrigidas para agrupamento lado a lado.
#'
#' @import lattice
#'
#' @references Aron Eklund (2016). beeswarm: The Bee Swarm Plot, an
#'     Alternative to Stripchart. R package version
#'     0.2.3. \url{https://CRAN.R-project.org/package=beeswarm}
#' @examples
#' library(lattice)
#'
#' set.seed(2016)
#' da <- data.frame(x = rep(letters[1:5], 10), y = rpois(50, 5))
#'
#' xyplot(y ~ x, data = da, jitter.x = TRUE)
#' xyplot(y ~ x, data = da, panel = panel.beeswarm, r = 0.1)
#' xyplot(y ~ x, data = da, panel = panel.beeswarm, r = 0.05)
#' 
#' xyplot(drat ~ carb | am, data = mtcars, jitter.x = TRUE)
#' xyplot(drat ~ carb | am, data = mtcars, panel = panel.beeswarm, r = 0.2)
#'

panel.beeswarm <- function(x, y, subscripts, r, ...) {
    xx <- x
    yy <- y
    aux <- by(cbind(yy, xx, subscripts), xx, function(i) {
        or <- order(i[, 1])
        ys <- i[or, 1]
        yt <- table(ys)
        dv <- sapply(unlist(yt), function(j) {
            seq(1, j, l = j) - (j + 1)/2
        })
        if (!is.list(dv)) {
            dv <- as.list(dv)
        }
        xs <- i[or, 2] + r * do.call(c, dv)
        cbind(x = xs, y = ys, subscripts[or])
    })
    aux <- do.call(rbind, aux)
    panel.xyplot(aux[, 1], aux[, 2], subscripts = aux[, 3], ...)
}

