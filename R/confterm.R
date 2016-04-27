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
#' @format Um \code{data.frame} com 176 observações e 4 variáveis, em
#'     que
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
#' @source MACHADO, N. S.; TINÔCO, I. D. F. F.; ZOLNIER, S.; MOGAMI,
#'     C. A.; DAMASCENO, F. A.; ZEVIANI, W. M. Resfriamento da cobertura
#'     de aviários e seus efeitos na mortalidade e nos índices de
#'     conforto térmico. Nucleus, La Habana, v.9, n.2, 2012.
#'     \url{http://www.nucleus.feituverava.com.br/index.php/nucleus/article/view/718},
#'     \url{http://www.ufv.br/dea/ambiagro/gallery/publicações/Artigo5.pdf}.
#' @examples
#'
#' library(lattice)
#'
#' str(confterm)
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
NULL
