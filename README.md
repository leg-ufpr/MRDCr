Modelos de Regressão para Dados de Contagem com o R
===================================================

Walmes Marques Zeviani\
Eduardo Elias Ribeiro Jr\
Cesar Augusto Taconeli

Resumo
------

Dados de contagens configuram variáveis aleatórias que assumem valores
inteiros não negativos.  Correspondem, possivelmente, ao primeiro tipo
de variável aleatória que o homem percebeu ou considerou para a tomada
de decisões, antes mesmo do surgimento da escrita.

Na era atual, métodos para inferência em dados de contagem estão bem
aquém da quantidade disponível para dados contínuos. Não é raro,
inclusive, que dados de contagem sejam analisados com base em modelos
para dados contínuos. Se por um lado existem muitos modelos disponíveis
nessa classe, por outro, seu uso para dados discretos é, na melhor das
hipóteses, uma aproximação sujeita a imperfeições relevantes.

O modelo Poisson é o principal e mais usado para inferência em dados de
contagem, estando disponível na maioria dos softwares Estatísticos e
grades curriculares. Apesar disso, reconhece-se que, na prática, as
suposições inerentes a esse modelo são frequentemente não atendidas, de
tal forma que, nessas situações, seu uso é não recomendado.

Contagens com excesso de zeros, sub ou super dispersão, limitadas,
censuradas, provenientes de experimentos/amostragem multinível, são
exemplos que demandam modelos mais flexíveis ou mais gerais que o
Poisson. No software R, vários pacotes disponibilizam alternativas
interessantes, prontamente disponíveis para uso, de modelos para dados
de contagens.

Uma vez que dados de contagem estão presentes em todas as áreas da
ciência (agronomia, ecologia, demografia, medicina, ciências sociais e
políticas, etc), relacionados a problemas de inferência, não se deve
subestimar a importância, mas estimular o conhecimento e emprego correto
de modelos de regressão apropriados para a análise de dados dessa
natureza.

Objetivos
---------

O objetivo desse Curso é apresentar diferentes modelos de regressão para
análise de dados de contagem, discutir suas principais propriedades e
ilustrar suas aplicações em dados reais por meio do software R. Dentre
os modelos considerados estão o modelo Poisson, algumas extensões para
acomodação de super (ou sub) dispersão (Quasi-Poisson,
COM-Poisson,Count-Gamma, Binomial-Nagativo), modelos para dados
inflacionados de zeros e com a inclusão de efeitos aleatórios.  Os
materiais do Curso (slides, dados, scripts) serão disponibilizados na
web.
