#' @name seguro
#' @title N\enc{ú}{u}mero de Sinistros por Cliente de Seguro de Ve\enc{í}{i}culo
#' @description Dados do número de sinistros em função do tempo de
#'     cobertura do seguro e características dos clientes.
#'
#' @format Um \code{data.frame} com 16644 observações e 6 variáveis, em
#'      que
#'
#' \describe{
#'
#' \item{\code{Idade}}{Idade do cliente (anos).}
#'
#' \item{\code{Sexo}}{Sexo do cliente.}
#'
#' \item{\code{Valor}}{Valor do veículo segurado (R\$).}
#'
#' \item{\code{Exposicao}}{Período de cobertura do cliente durante o ano
#'     sob análise (anos).}
#'
#' \item{\code{Sinistros}}{Número de sinistros registrados.}
#'
#' \item{\code{lexpo}}{Logarítimo neperiano da Exposição.}
#'
#' }
#' @examples
#'
#' library(lattice)
#'
#' tb <- xtabs(~Sinistros, data = seguro)
#' barchart(tb, horizontal = FALSE)
#'
#' tb <- xtabs(~Sinistros + Sexo, data = seguro)
#' barchart(tb, horizontal = FALSE, stack = FALSE, auto.key = TRUE)
#'
NULL
