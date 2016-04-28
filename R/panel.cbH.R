#' @name panel.cbH
#' @aliases panel.cbH
#' @export
#' @author Walmes Zeviani baseado na lista de discussão R-help.
#' @title Função para Intervalos e Bandas de Confiança com a Lattice
#'
#' @description Essa função permite representar intervalos de confiança
#'     e bandas de confiança em gráficos do pacote lattice.
#'
#' @param y Valor central ou estimativa pontual.
#'
#' @param ly Limite inferior do intervalo/banda de confiança.
#'
#' @param uy Limite superior do intervalo/banda de confiança.
#'
#' @param cty Uma string que indica o tipo de representação. Atualmente
#'     são aceitos os valores \code{"bars"} para intervalos de confiança
#'     e \code{"bands"} para bandas de confiança.
#'
#' @param desloc Um vetor númerico nos quais os valores representam
#'     quantidades a somar/subtrair dos valores de \code{x} para não
#'     sobrepor intervalos. Com esse argumento pode-se representar mais
#'     de um intervalor por valor de \code{x}. Não é usado quando
#'     \code{cty = "bands"}
#'
#' @param fill Uma representação de cor para preencher o interior das
#'     bandas de confiança. Não é usado quando \code{cty = "bars"}.
#'
#' @param alpha Nível de transparência na cor de preenchimento das
#'     bandas de confiança para permitir sobrepor bandas. O valor
#'     default é 0.1. Não é usado quando \code{cty = "bars"}.
#'
#' @param length Comprimento para as extremidades dos intervalos que
#'     formam o "T". O valor default é 0.05. Não é usado quando
#'     \code{cty = "bands"}.
#'
#' @param x,subscripts,col.line,lwd,... Argumentos passados para a
#'     função \code{\link[lattice]{xyplot}}.
#'
#' @return São usadas dentro de funções do pacote \pkg{lattice}.
#'
#' @seealso \code{\link{prepanel.cbH}}.
#' @import lattice latticeExtra
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' m0 <- lm(sqrt(dist) ~ speed, data = cars)
#' pred <- with(cars, data.frame(speed = seq(min(speed),
#'                                           max(speed),
#'                                           length.out = 20)))
#' aux <- predict(m0, newdata = pred, interval = "confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(sqrt(dist) ~ speed, data = cars) +
#'     as.layer(xyplot(fit ~ speed, data = pred, type = "l",
#'                     ly = pred$lwr, uy = pred$upr, cty = "bands",
#'                     fill = "blue", alpha = 0.3,
#'                     prepanel = prepanel.cbH,
#'                     panel = panel.cbH))
#'
#' m1 <- lm(weight ~ feed, data = chickwts)
#' pred <- with(chickwts, data.frame(feed = levels(feed)))
#' aux <- predict(m1, newdata = pred, interval = "confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(weight~feed, data=chickwts)+
#'     as.layer(xyplot(fit ~ feed, data = pred,
#'                     ly = pred$lwr, uy = pred$upr,
#'                     cty = "bars",
#'                     prepanel = prepanel.cbH,
#'                     desloc = rep(0.15, length(pred$fit)),
#'                     panel = panel.cbH))
#'
#' da <- expand.grid(trt = gl(2, 1), x = 1:7)
#' da$y <- with(da, as.integer(trt) + 0.5 * x + rnorm(x, 0, 0.4))
#' xyplot(y ~ x, groups = trt, data = da)
#'
#' m2 <- lm(y ~ trt + x, data = da)
#'
#' pred <- with(da, expand.grid(trt = levels(trt),
#'                              x = seq(min(x), max(x), length.out = 20)))
#' aux <- predict(m2, newdata = pred, interval = "confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(y ~ x, groups = trt, data = da) +
#'     as.layer(xyplot(fit ~ x, groups = trt, data = pred, type = "l",
#'                     ly = pred$lwr, uy = pred$upr,
#'                     cty = "bands", alpha = 0.25,
#'                     prepanel = prepanel.cbH,
#'                     panel = panel.superpose,
#'                     panel.groups = panel.cbH))
#'
panel.cbH <- function(x, y, ly, uy,
                      subscripts, cty,
                      col.line = plot.line$col,
                      lwd = plot.line$lwd,
                      desloc = NULL,
                      fill = 1, alpha = 0.1, length = 0.05, ...) {
    plot.line <- trellis.par.get("plot.line")
    if (is.null(desloc))
        desloc <- rep(0, length(uy))
    y <- as.numeric(y)
    x <- as.numeric(x)
    or <- order(x)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    xo <- x[or]
    yo <- y[or]
    lyo <- ly[or]
    uyo <- uy[or]
    desl <- desloc[subscripts]
    if (cty == "bands") {
        panel.polygon(c(xo, rev(xo)), c(lyo, rev(uyo)), col = fill,
                      alpha = alpha, border = NA)
        panel.lines(xo, lyo, lty = 3, lwd = 0.5, col = col.line)
        panel.lines(xo, uyo, lty = 3, lwd = 0.5, col = col.line)
    }
    if (cty == "bars") {
        panel.arrows(xo + desl, lyo, xo + desl, uyo, length = length,
                     code = 3, angle = 90, col = col.line, lwd = lwd)
    }
    panel.xyplot(x + desl, y, subscripts = subscripts,
                 col.line = col.line, lwd = lwd, ...)
}

#' @name prepanel.cbH
#' @rdname panel.cbH
#' @author Walmes Zeviani baseado na lista de discussão R-help.
#' @import lattice latticeExtra
#' @export
prepanel.cbH <- function(y, ly, uy, subscripts){
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    y <- as.numeric(y[subscripts])
    list(ylim=range(y, uy, ly, finite=TRUE))
}
