#' @name seguros
#' @title Número de Sinistros em uma Seguradora de Automóveis
#' @description Dados referentes ao acompanhamento de clientes de uma
#'     seguradora de automóveis ao longo de um ano. Foram registrados as
#'     variáveis descritas abaixo para 16483 clientes.
#' @format Um \code{data.frame} com 16483 observações e 7 variáveis.
#'     \describe{
#' 
#' \item{\code{tipo}}{Tipo de veículo segurado. Fator com seis níveis
#'     \code{hatch}, \code{mono}, \code{picape}, \code{sedan},
#'     \code{wagon} e \code{suv}.}
#' 
#' \item{\code{idade}}{Idade do cliente, em anos.}
#' 
#' \item{\code{sexo}}{Sexo do cliente.}
#' 
#' \item{\code{civil}}{Estado civil do cliente. Fator com dois níveis,
#'     \code{M} para clientes do sexo masculino e \code{F} para
#'     feminino.}
#' 
#' \item{\code{valor}}{Valor do veículo segurado, em 1000 reais.}
#' 
#' \item{\code{expos}}{Período de cobertura do cliente durante o ano sob
#'     análise.}
#' 
#' \item{\code{nsinist}}{Número de sinistros registrados no período.}
#' 
#' }
#' @keywords excesso-zeros
#' @examples
#' data(seguros)
#' 
#' str(seguros)
#' 
#' summary(seguros)
NULL

