#!/usr/bin/env Rscript

options(echo = TRUE)

#--------------------------------------------
# Carregar pacotes.

# - Para desenvolvimento do pacote
library(devtools)
library(roxygen2)

# - Instalando as dependencias (descritas no DESCRIPTION)
install_deps(dependencies = TRUE,
             quiet = TRUE,
             upgrade = FALSE,
             repos = "http://cran-r.c3sl.ufpr.br/")

sessionInfo()

#--------------------------------------------
# Carregar objetos do pacote.

file.create("NAMESPACE")

load_all()

ls("package:MRDCr")
packageVersion("MRDCr")

#--------------------------------------------
# Ignora check de caracteres non-ASCII nos datasets
Sys.setenv(`_R_CHECK_ASCII_DATA_` = "FALSE")

#--------------------------------------------
# Produzir a documentação dos objetos.

document()

cp <- compareVersion(a = as.character(packageVersion("devtools")),
                     b = "1.9.1")

if (cp > 0) {
    check_man()
} else {
    check_doc()
}

#--------------------------------------------
# Gerar as vinhetas, caso existam.

if (length(list.files("./vignettes"))) {
    build_vignettes(dependencies = FALSE)
}

#--------------------------------------------
# Checar conteúdo e organização do pacote.

check(cleanup = FALSE,
      manual = TRUE,
      vignettes = FALSE,
      check_dir = "../")

#--------------------------------------------
# Construir pacote.

build(manual = TRUE, vignettes = TRUE)

#--------------------------------------------
# Instalar o pacote.

rm(list = ls())

lib <- path.expand("~/R-test/")
dir.create(lib)
.libPaths(new = lib)
.libPaths()

install()

unlink(lib, recursive = TRUE)

#--------------------------------------------
