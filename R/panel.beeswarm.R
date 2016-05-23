#' @name panel.beeswarm
#' @export
#' @author Walmes Zeviani baseado no pacote
#'     \href{http://www.cbs.dtu.dk/~eklund/beeswarm/}{beeswarm}.
#' @title Diagrama de Dispersão com Aranjo dos Pontos como Colmeia
#'
#' @description Used to make scatter plot of discrete variables with no
#' overlapping points. Observations with the same y value are spread.
#'
#' @param spread Um escalar numérico a distância entre os pontos. Esse
#'     valor é obtido por tentativa erro e toda vez que mudar as
#'     dimensões do gráfico, eles precisam ser novamente fornecidos, no
#'     entanto são valores na escala do eixo \code{x} e por isso são
#'     baseados nas distâncias entre os níveis do fator representado
#'     neste eixo. Como sugestão, abra sempre a janela gráfica
#'     (\code{x11()}) ou faça a exportação (\code{png()}, \code{pdf()},
#'     etc) com dimensões conhecidas e calibre o \code{spred} para que
#'     seja exibido adequadamente.
#'
#' @param x,y,subscripts,... Argumentos passados para a
#'     \code{\link[lattice]{panel.xyplot}}.
#'
#' @return A função passa conteúdo para o argumento \code{panel}.
#'
#' @seealso \code{\link[lattice]{xyplot}}.
#' @import lattice
#' @examples
#'
#' data(capdesfo)
#' str(capdesfo)
#'
#' library(lattice)
#'
#' # x11(width = 7, height = 2.8)
#' xyplot(ncap ~ des | est, data = capdesfo,
#'        layout = c(5, 1), as.table = TRUE,
#'        type = c("p", "smooth"), col = 1, col.line = "gray50",
#'        xlim = extendrange(c(0:1), f = 0.15),
#'        xlab = "Nível de desfolha artificial",
#'        ylab = "Número de capulho produzidos",
#'        spread = 0.07, panel = panel.beeswarm)
#'
#' # x11(width = 7, height = 2.8)
#' xyplot(ncap ~ est | factor(des), data = capdesfo,
#'        layout = c(5, 1), as.table = TRUE,
#'        type = c("p", "smooth"), col = 1, col.line = "gray50",
#'        xlab = "Fase de desenvolvimento da planta",
#'        ylab = "Número de capulhos produzidos",
#'        scales = list(x = list(
#'                          at = 1:nlevels(capdesfo$est),
#'                          labels = substr(levels(capdesfo$est),
#'                                          start = 1, stop = 3))),
#'        spread = 0.35, panel = panel.beeswarm)
#'
panel.beeswarm <- function(x, y, subscripts, spread, ...){
    xx <- x
    yy <- y
    aux <- by(cbind(yy, xx, subscripts),
              INDICES = xx,
              FUN = function(i) {
                  or <- order(i[, 1])
                  ys <- i[or, 1]
                  yt <- table(ys)
                  dv <- sapply(unlist(yt),
                               FUN = function(j) {
                                   seq(from = 1,
                                       to = j,
                                       length.out = j) -
                                       (j + 1)/2
                               })
                  if (!is.list(dv)) {
                      dv <- as.list(dv)
                  }
                  xs <- i[or, 2] + spread * do.call(c, dv)
                  cbind(x = xs, y = ys, subscripts = subscripts[or])
              })
    aux <- do.call(rbind, aux)
    panel.xyplot(aux[, 1], aux[, 2], subscripts = aux[, 3], ...)
}
