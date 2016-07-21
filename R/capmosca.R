#' @name capmosca
#' @title Capulhos de Algod\enc{ã}{a}o em Fun\enc{çã}{ca}o da Exposi\enc{çã}{ca}o \enc{à}{a} Mosca Branca
#' @description Experimento conduzido na Universidade Federal da Grande
#'     Dourados (UFGD) em 2007, cujo objetivo foi avaliar os impactos da
#'     exposição de plantas à alta infestação de Mosca-Branca
#'     \emph{Bemisia tabaci} em componentes de produção do algodão. No
#'     experimento, plantas de algodão foram expostas à alta infestação
#'     da praga por períodos diferentes e ao final do experimento
#'     avaliou-se o número de capulhos produzidos, o número de
#'     estruturas reprodutivas, o número de nós, a altura da planta e o
#'     peso dos capulhos por vaso. A condução do estudo deu-se via
#'     delineamento interamente casualizado com 5 vasos, contendo duas
#'     plantas, para cada período de exposição.
#' @format Um \code{data.frame} com 60 observações e 8 variáveis.
#'     \describe{
#'
#' \item{\code{dexp}}{Inteiro com 6 valores que representa os dias de
#'     exposição à alta infestação de Mosca-Branca.}
#'
#' \item{\code{vaso}}{Fator que indica o vaso no qual foram mensurados
#'     os componentes de produção do algodão.}
#'
#' \item{\code{planta}}{Fator que indica a planta na qual foram
#'     mensurados os componentes de produção do algodão.}
#'
#' \item{\code{alt}}{Altura da planta, mensurada em centímetros.}
#'
#' \item{\code{pesocap}}{Peso dos capulhos de algodão, mesurado para
#'     cada vaso (que contém duas plantas). No \code{data.frame} somente
#'     a primeira planta do vaso contém a observação do peso.}
#'
#' \item{\code{nerep}}{Contagem do número de estruturas reprodutivas da
#'     planta.}
#'
#' \item{\code{ncap}}{Contagem do número de capulhos produzidos.}
#'
#' \item{\code{nnos}}{Contagem do número de nós da planta.}
#'
#' }
#'
#' @examples
#' data(capmosca)
#' str(capmosca)
#'
#' library(lattice)
#'
#' # Número de capulhos produzidos por vaso
#' da <- aggregate(ncap ~ vaso + dexp, data = capmosca, FUN = sum)
#' xyplot(ncap ~ dexp,
#'        data = da,
#'        jitter.x = TRUE,
#'        type = c("p", "g", "smooth"))
#'
#' # Número de capulhos produzidos por planta
#' xyplot(ncap ~ dexp, groups = planta,
#'        data = capmosca,
#'        jitter.x = TRUE,
#'        type = c("p", "g", "smooth"))
#'
#' # Número de estruturas reprodutivas da planta
#' xyplot(nerep ~ dexp, groups = planta,
#'        data = capmosca,
#'        jitter.x = TRUE,
#'        type = c("p", "g", "smooth"))
#'
#' # Número de nós da planta
#' xyplot(nnos ~ dexp, groups = planta,
#'        data = capmosca,
#'        jitter.x = TRUE,
#'        type = c("p", "g", "smooth"))
#'
NULL
