% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmp.R
\name{llcmp}
\alias{llcmp}
\title{Log-Verossimilhança do Modelo Conway-Maxwell-Poisson}
\usage{
llcmp(params, y, X, sumto = ceiling(max(y)^1.2))
}
\arguments{
\item{params}{Um vetor de parâmetros do modelo COM-Poisson. O
primeiro elemento desse vetor deve ser o parâmetro de dispersão
do modelo, \eqn{\phi}, os restantes são os parâmetros
\eqn{\beta}'s associados ao preditor linear em \eqn{\lambda}.}

\item{y}{Um vetor com variável dependente do modelo, resposta do tipo
contagem.}

\item{X}{A matriz de delineamento correspondente ao modelo linear
ligado à \eqn{\lambda} pela função de ligação log. A matriz do
modelo pode ser construída com a função
\code{\link[stats]{model.matrix}}.}

\item{sumto}{Número de incrementos a serem considerados para a
cálculo das constantes normalizadoras. Como padrão, o número de
incrementos é o valor inteiro de \eqn{(\max y)^1.5}, porém esse
valor padrão não é ótimo. Uma avaliação da escolha desse
argumento, pós ajuste pode ser realizada via
\code{\link[MRDCr]{convergencez}}.}
}
\value{
O negativo da log-verossimilhança do modelo
    Conway-Maxwell-Poisson com os parâmetros e dados informados.
}
\description{
Calcula a log-verossimilhança de um modelo de regressão
    para o parâmetro \eqn{\lambda} considerando as respostas de
    contagem (y), condicionadas as suas covariáveis (X), distribuídas
    conforme modelo COM-Poisson.
}
\details{
A função de log-verossimilhança da COM-Poisson, na
    parametrização de modelo de regresssão é:

\deqn{\ell(\beta, \nu, y) =
    \sum_{i=1}^{n} y_i \log(\lambda_i) - \nu \sum_{i=1}^{n}\log(y!)
    - \sum_{i=1}^{n} \log(Z(\lambda_i, \nu))}

em que (i) \eqn{\lambda_i = \exp(X_i \beta)}, no modelo de regressão
    COM-Poisson um preditor linear é ligado à \eqn{\lambda} por meio
    da função de ligação log. Note que não estamos modelando
    diretamente a média, assim as estimativas dos parâmetros
    \eqn{\beta} não tem a mesma interpretação dos modelos Poisson,
    por exemplo. Contudo, os sinais desses parâmetros indicam efeitos
    de acréscimo ou descréscimo nas contagens médias.
(ii) \eqn{\nu} é o parâmetro de dispersão que indica equi, sub ou
    superdispersão das contagens y. Sendo \eqn{nu = 1} o caso de
    equidispersão, \eqn{0 \leq \nu < 1} superdispersão e \eqn{\nu >
    1} subdispersão. Vale ressaltar que a variância \eqn{V(Y)} não
    tem expressão fechada e não é definada unicamente por \eqn{\nu}.
(iii) \eqn{Z(\lambda_i, \nu)} é a constante de normalização definida
    por \deqn{\sum_{j=0}^{\infty} \frac{\lambda_i^j}{(j!)^\nu}}. Note
    que são cálculadas n constantes Z. Nesta implementação o número
    de incrementos considerados para cálculo dessas constantes é
    definido por \code{sumto}, o mesmo número de incrementos é
    considerado para o cálculo de todas as contantes. Uma verificação
    pós ajuste da escolha de \code{sumto} pode ser realizada a partir
    de \code{\link[MRDCr]{convergencez}}.

Nesta parametrização o modelo COM-Poisson tem como casos particulares
    os modelos Poisson quando \eqn{\nu = 1}, Bernoulli quando
    \eqn{\nu \rightarrow \infty} (ou o modelo logístico considerando
    modelos de regressão) e Geométrico quando \eqn{\nu = 0} e
    \eqn{\lambda < 1}.

Para que não seja necessário restringir o algoritmo de maximização da
    log-verossimilhança, a função foi implementada reparametrizando o
    parâmetro \eqn{\nu} para \eqn{\log(\phi)}. Assim o parâmetro
    estimado será \eqn{\phi} que tem suporte nos reais, assim como o
    vetor \eqn{\beta}.
}
\seealso{
\code{\link[bbmle]{mle2}}
}
\author{
Eduardo E. R. Junior, \email{edujrrib@gmail.com}
}
