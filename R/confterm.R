#' @name confterm
#' @title Resfriamento da Cobertura de Aviários na Mortalidade de Aves
#' @description Resultados de um experimento que estudou o efeito do
#'     sistema de refriamento de cobertura de aviáros sob a mortalidade
#'     das aves e o conforto térmico. Haviam 4 aviários disponíveis e
#'     por sorteio, em dois deles foi instalado o sistema de
#'     resfriamento de cobertura, cuja finalidade é diminuir a
#'     temperatura nas horas mais quentes do dia para evitar morte de
#'     aves por calor. Nesse experimento, o sistema só foi utilizado a
#'     partir dos 21 dias de idade. A cada dia foi contado o número de
#'     aves encontradas mortas no aviário.
#'
#' Fora dos galpões, um sistema de monitoramento das variáveis
#'     ambientais registrou, em intervalos de 1 hora dos 21 aos 39 dias
#'     de idade, as variáveis para que fossem determinados: a entalpia
#'     específica do ar (H), a carga térmica de radiação (CTR) e o
#'     índice de temperatura de globo negro e umidade (ITGU). Essas
#'     variáveis tem a finalidade de explicar a variação da mortalidade
#'     das aves nos sistemas de resfriamento ao longo dos dias.
#' @format \code{confterm} é um \code{data.frame} com 176 observações e
#'     4 variáveis, em que
#'
#' \describe{
#'
#' \item{\code{resfr}}{Variável que indica se o galpão tem ou não
#'     sistema de resfriamento de cobertura (C = com refriamento de
#'     cobertura, S = sem refriamento de cobertura).}
#'
#' \item{\code{idade}}{Idade das aves, em dias após o alojamento.}
#'
#' \item{\code{galp}}{Variável que indica o galpão que é a unidade
#'     experimental para o efeito do sistema de refriamento de
#'     cobertura.}
#'
#' \item{\code{nap}}{Número de aves perdidas (ou mortas) por dia.}
#'
#' }
#'
#' \code{conftemp} é um \code{data.frame} com 456 observações e 6
#'     variáveis, em que
#'
#' \describe{
#'
#' \item{\code{hora}}{As horas em cada dia, retomando do 0 em cada novo
#'     dia.}
#'
#' \item{\code{hr}}{As horas a partir o primeiro dia continuamente.}
#'
#' \item{\code{idade}}{A idade dos animais, em dias.}
#'
#' \item{\code{h}}{Entalpia específica do ar.}
#'
#' \item{\code{ctr}}{Carga térmica de radiação.}
#'
#' \item{\code{itgu}}{Índice de temperatura de globo negro e umidade.}
#'
#' }
#' @source MACHADO, N. S.; TINÔCO, I. D. F. F.; ZOLNIER, S.; MOGAMI,
#'     C. A.; DAMASCENO, F. A.; ZEVIANI, W. M. Resfriamento da cobertura
#'     de aviários e seus efeitos na mortalidade e nos índices de
#'     conforto térmico. Nucleus, La Habana, v.9, n.2, 2012.
#'     \url{http://www.nucleus.feituverava.com.br/index.php/nucleus/article/view/718},
#'     \url{http://www.ufv.br/dea/ambiagro/gallery/publicações/Artigo5.pdf}.
#' @examples
#'
#' #-----------------------------------------
#' # Gráfico da mortalidade das aves.
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' str(confterm)
#' summary(confterm)
#'
#' xtabs(~idade + resfr, data = confterm)
#'
#' xyplot(nap ~ idade | resfr, data = confterm,
#'        groups = galp, type = "o",
#'        xlab = "Idade das aves (dias)",
#'        ylab = "Número de aves perdidas por galpão",
#'        strip = strip.custom(factor.levels = c(
#'                                 "Com sistema de resfriamento",
#'                                 "Sem sistema de resfriamento")),
#'        auto.key = list(corner = c(0.05, 0.9)))
#'
#' #-----------------------------------------
#' # Gráfico das variáveis térmicas.
#'
#' # Amplitude estendida das variáveis.
#' lim <- with(conftemp, apply(cbind(h, ctr, itgu), MARGIN = 2,
#'                             FUN = extendrange, f = 0.2))
#'
#' # Anotação da eixo x do gráfico.
#' scales <- list(
#'     y = list(relation = "free"),
#'     x = list(at = seq(from = 1,
#'                       to = ceiling(max(conftemp$hr/24)) * 24,
#'                       by = 24)))
#' scales$x$labels <- seq_along(scales$x$at)
#'
#' xyplot(h + ctr + itgu ~ hr, data = conftemp,
#'        outer = TRUE, type = "l", layout = c(1, NA),
#'        scales = scales, xlim = range(scales$x$at),
#'        xlab = "Dias",
#'        ylab = "Variáveis térmicas",
#'        panel = function(y, subscripts, ...) {
#'            wp <- which.packet()
#'            r <- lim[, wp[1]]
#'            panel.rect(10.5 + 24 * (scales$x$labels - 1), r[1],
#'                       20 + 24 * (scales$x$labels - 1), r[2],
#'                       col = "blue",
#'                       border = "transparent",
#'                       alpha = 0.25)
#'            panel.xyplot(y = y, subscripts = subscripts, ...)
#'        })
#'
#' # Valores máximos do dia.
#' tempdia <- aggregate(cbind(hm = h, cm = ctr, im = itgu) ~ idade,
#'                      data = conftemp, FUN = max)
#'
#' splom(tempdia[, -1])
#'
#' confterm <- merge(confterm, tempdia, by = "idade")
#' str(confterm)
#'
NULL

#' @name conftemp
#' @rdname confterm
NULL
