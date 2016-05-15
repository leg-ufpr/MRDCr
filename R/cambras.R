#' @name cambras
#' @title Resultados do Campeonato Brasileiro de 2010
#' @description Dados sobre o número de gols dos times mandantes e
#'     desafiantes de todas as partidas do Campeonato Brasileiro de
#'     2010.
#' @format Um \code{data.frame} com 380 observações e 5 variáveis.
#' \describe{
#'
#' \item{\code{rod}}{Inteiro que identifica o número da rodada.}
#'
#' \item{\code{home}}{Fator que identifica o time mandante da partida,
#'     aquele que jogou em casa.}
#'
#' \item{\code{away}}{Fator que identifica o time desafiante da partida,
#'     aquele que jogou fora de casa.}
#'
#' \item{\code{h}}{Número de gols feitos pelo time mandante na partida.}
#'
#' \item{\code{a}}{Número de gols feitos pelo time desafiante na
#'     partida.}
#'
#' }
#' @examples
#'
#' # Resultados finais na internet.
#' # browseURL(paste0("http://esportes.terra.com.br/futebol/",
#' #                  "brasileiro/2010/noticias/0,,OI4339585-EI15413",
#' #                  ",00-Classificacao+e+Jogos+Serie+A.html"))
#'
#' Xh <- model.matrix(~-1 + home, cambras)
#' Xa <- model.matrix(~-1 + away, cambras)
#'
#' Xha <- Xh - Xa
#' cambras[1:5, c("home", "away")]
#' print(as.table(t(Xha[1:5, ])), zero.print = ".")
#'
#' gsgc <- cbind(scored = t(Xh) %*% cambras$h + t(Xa) %*% cambras$a,
#'               conceded = t(Xa) %*% cambras$h + t(Xh) %*% cambras$a)
#' colnames(gsgc) <- c("gScored", "gConced")
#' gsgc
#'
#' # Pontos em cada partida (empate = 1, vitória = 3).
#' pts <- with(cambras, {
#'     d <- h - a
#'     draw <- d == 0
#'     winH <- d > 0
#'     winA <- !(draw | winH)
#'     x <- cbind(h = winH * 3 + draw, a = winA * 3 + draw)
#'     return(x)
#' })
#'
#' tableIn <- function(x) {
#'     tb <- table(x)
#'     f <- rep(0, 3)
#'     names(f) <- c(0, 1, 3)
#'     f[names(tb)] <- tb
#'     return(f)
#' }
#'
#' # Derrotas, empates e vitórias.
#' ldw <-
#'     do.call(rbind, lapply(tapply(pts[, 1], cambras$home,
#'                                  FUN = tableIn),
#'                           FUN = as.vector)) +
#'     do.call(rbind, lapply(tapply(pts[, 2], cambras$away,
#'                                  FUN = tableIn),
#'                           FUN = as.vector))
#' colnames(ldw) <- c("Lose", "Draw", "Win")
#' ldw
#'
#' # Tabela completa de 2010.
#' pl10 <- t(Xh) %*% pts[, "h"] + t(Xa) %*% pts[, "a"]
#' pl10 <- cbind(team = levels(cambras$home), data.frame(pts = pl10))
#' pl10 <- cbind(pl10, ldw[, 3:1], gsgc)
#' pl10 <- plyr::arrange(pl10, -pts)
#' pl10$pos <- rank(-pl10$pts)
#' pl10
#'
NULL
