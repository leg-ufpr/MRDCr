#' @name peixe
#' @title Peixes Capturados por Pescadores em um Parque Estadual
#' @description Dados sobre 250 grupos que foram ao parque ao parque
#'     estadual para pescar. As informações coletadas foram refentes a
#'     presenção ou não de um campista, ao número de crianças no grupo e
#'     ao número de indivíduos no grupo.
#' @format Um \code{data.frame} com 250 observações e 4 variáveis.
#'     \describe{
#' 
#' \item{\code{campista}}{Fator com dois níveis que representa a
#'     presença (\code{1}) ou ausência (\code{0}) de um campista no
#'     grupo.}
#' 
#' \item{\code{ncriancas}}{Número de crianças no grupo.}
#' 
#' \item{\code{npessoas}}{Número total de pessoas no grupo.}
#' 
#' \item{\code{npeixes}}{Número de peixes capturados pelo grupo.}
#' 
#' }
#'
#' @references Calvin, J. A. (1998). Regression Models for Categorical
#'     and Limited Dependent Variables. Technometrics, 40(1), 80-81.
#' @examples
#'
#' data(peixe)
#' (proptb <- prop.table(table(peixe$npeixes)))
#' plot(proptb)
#' 
#' library(lattice)
#' # Contagens (marginal aos efeitos das covariáveis)
#' histogram(~npeixes, data = peixe, nint = 50)
#' 
#' # Contagens com relação as covariáveis
#' xyplot(npeixes ~ ncriancas + npessoas,
#'        groups = campista,
#'        data = peixe,
#'        jitter.x = TRUE,
#'        type = c("p", "g", "smooth"))
#'
NULL
