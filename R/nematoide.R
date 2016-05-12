#' @name nematoide
#' @title Número de Nematóides em Raízes de Feijoeiro
#' @description Resultados de um experimento em casa de vegetação que
#'     estudou a reprodução de nematóides em cultivares/linhagens de
#'     feijoeiro. O solo dos vasos foi inicialmente contaminado com
#'     namatóides e as parcelas tiveram duas plantas. Ao final do
#'     experimento, as raízes das duas plantas por parcela foram
#'     lavadas, trituradas, peneiradas e diluídas para fazer a contagem
#'     dos nematóides em aliquotas dessa solução.
#' @format Um \code{data.frame} com 94 observações e 4 variáveis.
#'
#' \describe{
#'
#' \item{\code{cult}}{Fator categórico que indica a linhagem de
#'     feijoeiro semeada em vasos com solo contaminado com nematóide.}
#'
#' \item{\code{mfr}}{Massa fresca de raízes (g) produzida por parcela
#'     (duas plantas) que foi lavada, triturada, peneirada e diluída
#'     para fazer a contagem dos nematóides.}
#'
#' \item{\code{vol}}{Volume (ml) usado para diluir a massa fresca de
#'     raízes. Esse volume foi agitado para homogeneização e depois uma
#'     alíquota de 1 ml foi extraída e colocada em uma lâmina de
#'     contagem.}
#'
#' \item{\code{nema}}{Número de nematóides na alíquota de 1 ml,
#'     determinado por contagem direta na lâmina.}
#'
#' \item{\code{off}}{É o offset da contagem, o equivalente em massa de
#'     fresca de raízes de uma aliquota de 1 ml, ou seja, é
#'     \code{off = mfr/vol}.}
#'
#' }
#' @source Cedido para fins acadêmicos por Andressa Cristina Zamboni
#'     Machado, pesquisadora do Instituto Agronômico do Paraná (IAPAR),
#'     e pelo técnico agrícola do IAPAR Santino Aleandro da Silva.
#'
#' O nome das cultivares, a espécie do nematóide e outras informações
#'     não foram dadas para preservar a originalidade da Pesquisa.
#' @examples
