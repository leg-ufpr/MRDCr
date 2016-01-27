Modelos de Regressão para Dados de Contagem com o R
===================================================

Cesar Augusto Taconeli
Eduardo Elias Ribeiro Jr
Walmes Marques Zeviani

Resumo
------

Contagens são variáveis aleatórias que assumem valores inteiros não
negativos.  Possivelmente foi a primeira variável aleatória que o homem
percebeu ou considerou para tomar decisões, antes mesmo do surgimento da
escrita.

Na era atual, métodos para inferência em dados de contagem estão bem
aquém da quantidade disponível para dados contínuos. Não é raro,
inclusive, que dados de contagem sejam analisados por modelos para dados
contínuos. Se por um lado existem muitos modelos disponíveis nessa
classe, por outro, seu uso para dados discretos é, na melhor das
intenções, uma aproximação sujeita a imperfeições relevantes.

O modelo Poisson é o principal e mais usado para inferência em dados de
contagem. Ele esta disponível na maioria dos softwares Estatísticos e
grades curriculares. Apesar disso, reconhece-se que as suas suposições
são frequentemente não atendidas em muitas aplicações e, nessas
situações, seu uso é não recomendado.

Contagens com excesso de zeros, sub ou super dispersão, limitadas,
censuradas, provenientes de experimentos/amostragem multinível, são
exemplos que demandam modelos mais flexíveis ou mais gerais que o
Poisson. No software R, vários pacotes disponibilizam alternativas
interessantes, prontamente disponível para uso, de modelos para dados de
contagem.

Como todas as áreas da ciência têm dados de contagem (agronomia,
ecologia, demografia, medicina, ciências sociais e políticas, etc)
relacionados a problemas de inferência, não se deve subestimar a
importância mas estimular o conhecimento e emprego correto dos modelos
de regressão.

Objetivos
---------

O objetivo desse Curso é apresentar modelos de regressão para análise de
dados de contagem e ilustrar suas aplicações. Serão considerados o
modelo Poisson, algumas extensões (Quasi-Poisson, COM-Poisson,
Count-Gamma, Binomial-Nagativo), modelos para inflação de zeros e
efeitos aleatórios. As aplicações serão todas feitas em R e os materiais
do Curso (slides, dados, scripts) serão disponibilizados na web.
