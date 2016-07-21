#' @name postura
#' @title N\enc{ú}{u}mero de Trocas de Postura em Ovelhas
#' @description Os dados são referentes a um experimento realizado com o
#'     objetivo de investigar o efeito de uma intervenção, por parte do
#'     cuidador, no comportamento de ovelhas.
#'
#' Para isso, foram consideradas ovelhas de duas linhagens distintas
#'     (pouco ou muito reativas), submetidas a dois tipos diferentes de
#'     intervenção (observação ou observação + intervenção).
#'
#' A variável resposta considerada é o número de mudanças na postura
#'     corporal do animal ao longo do período de observação (3 minutos).
#'
#' @format Um \code{data.frame} com 38 observações e 3 variáveis, em
#'      que
#'
#' \describe{
#'
#' \item{\code{trat}}{Fator com dois níveis que representa a intervenção
#'     feita.}
#'
#' \item{\code{linh}}{Fator que representa a linhagem das ovelhas.}
#'
#' \item{\code{npost}}{Número de trocas de postura em 3 minutos.}
#'
#' }
#' @examples
#'
#' tb <- xtabs(~trat + linh, data = postura)
#' tb
#'
#' mosaicplot(tb)
#'
NULL
