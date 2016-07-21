Modelos de Regressão para Dados de Contagem com o R
===================================================

[Walmes Marques Zeviani], [Eduardo Elias Ribeiro Jr] & [Cesar Augusto
Taconeli]

Resumo do Curso
---------------

Dados de contagens são variáveis aleatórias que assumem valores inteiros
não negativos.  Correspondem, possivelmente, ao primeiro tipo de
variável aleatória que o homem percebeu ou considerou para a tomada de
decisões, antes mesmo do surgimento da escrita.

Na era atual, métodos para inferência em dados de contagem estão bem
aquém da quantidade disponível para dados contínuos. Não é raro,
inclusive, que dados de contagem sejam analisados com base em modelos
para dados contínuos. Se por um lado existem muitos modelos disponíveis
nessa classe, por outro, seu uso para dados discretos é, na melhor das
hipóteses, uma aproximação sujeita a imperfeições relevantes.

O modelo Poisson é o principal e mais usado para inferência em dados de
contagem, estando disponível na maioria dos softwares Estatísticos e
grades curriculares de Cursos de (Pós)-Graduação ou especialização em
Estatística. Apesar disso, reconhece-se que, na prática, as suposições
inerentes a esse modelo são frequentemente não atendidas, de tal forma
que, nessas situações, seu uso não é recomendado.

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

Objetivos do Curso
------------------

O objetivo desse Curso é apresentar diferentes modelos de regressão para
análise de dados de contagem, discutir suas principais propriedades e
ilustrar suas aplicações em dados reais por meio do software R. Dentre
os modelos considerados estão o modelo Poisson, algumas extensões para
acomodação de super (ou sub) dispersão (Quasi-Poisson,
COM-Poisson,Count-Gamma, Binomial-Nagativo), modelos para dados
inflacionados de zeros e com a inclusão de efeitos aleatórios.  Os
materiais do Curso (slides, dados, scripts) serão disponibilizados no
pacote MRDCr - Modelos de Regressão Para Dados de Contagem.

Organização e Uso do Pacote
---------------------------

Este repositório é um pacote R. Dentro dele estão os dados que foram
usados no curso, todos documentados, e as análises feitas com esses
dados (vinhetas). As funções escritas que implementam modelos
alternativos também estão no pacote bem como os slides usados no
Curso. Execute os comandos abaixo para fazer um *tour* pelo pacote
MRDCr.

```r
# Carrega o pacote.
library(MRDCr)

# Lista os objetos do pacote.
ls("package:MRDCr")

# Abre a documentação do pacote.
help(package = "MRDCr", help_type = "html")

# Abre a lista de vinhetas no navegador.
browseVignettes(package = "MRDCr")

# Exibe a lista no console.
vignette(package = "MRDCr")

# Abre uma das vinhetas.
vignette(topic = "v01_poisson", package = "MRDCr")

# Citação do pacote.
citation("MRDCr")

# Caminho para o diretório com slides do MRDCr.
sld <- system.file("slides", package = "MRDCr")
dir(sld)

# Caminho para os slides.
paste0(sld, "/slides-mrdcr.pdf")

# Usuários Linux podem abrir os slides assim.
system(paste0("evince ", sld, "/slides-mrdcr.pdf"))

# Descrição do pacote, com contato dos autores.
packageDescription("MRDCr")
```

Instalação do Pacote
--------------------

O pacote pode ser instalado diretamente dos repositório em que está
hospedado no [GitHub]: <https://github.com/leg-ufpr/MRDCr>. Para isso é
necessário ter o pacote `devtools`. Execute o código abaixo para
instalar o pacote.

```r
library(devtools)

library(devtools)
install_github(repo = "MRDCr",
               username = "leg-ufpr",
               ref = "devel")
```

Caso não consiga instalar o pacote, tente fazer a partir dos aquivos
comprimidos disponíveis em <http://leg.ufpr.br/~walmes/pacotes/>. Se
você usa Windows, use o `zip`, se Linux use o `tar.gz`. O código abaixo
mostra como fazer a instalação.

```
# Instalando a partir do tarball (Linux).
install.packages(pkgs = "MRDCr_0.0-2.tar.gz",
                 repos = NULL, type = "source")

# Instalando a partir do zip (Windows).
install.packages(pkgs = "MRDCr_0.0.2.zip",
                 repos = NULL, type = "source")
```

Lembre-se que esse procedimento não instala as dependências do
pacote. Portanto, na hora de usar pode ser dado falta de algo. Para
evitar isso, antes de instalar, procure instalar todos os pacotes que
dependências para o MRDCr. Eles são listadoss no arquivo [DESCRIPTION].

Agradecimentos
--------------

O pacote MRDCr foi feito como material do Curso **Modelos de Regressão
para Análise de Dados de Contagem** que foi aceito para o [62 RBRAS],
que aconteceu em Salvador - BA no período de 23 a 25 de Maio
de 2016. Portanto agradecemos à Organização da 61 RBRAS pela
oportunidade cedida para oferta deste Curso.

O Curso foi baseado em estudos de casos feito com conjuntos de dados
reais. Agradecemos às pessoas que permitiram que os dados fossem
utilizados pois os dados foram o substrato base para a construção deste
Curso.

<!------------------------------------------- -->

[Walmes Marques Zeviani]: http://leg.ufpr.br/~walmes
[Eduardo Elias Ribeiro Jr]: https://jreduardo.github.io/
[Cesar Augusto Taconeli]: https://docs.ufpr.br/~taconeli/
[GitHub]: https://github.com/leg-ufpr/MRDCr
[62 RBRAS]: http://rbras2016.ufba.br/pt/
[DESCRIPTION]: https://github.com/leg-ufpr/MRDCr/blob/devel/DESCRIPTION
