.onAttach <- function (lib, pkg) {
    pkg.info <- drop(read.dcf(
        file = system.file("DESCRIPTION", package = "MRDCr"),
        fields = c("Package", "Title", "Version", "Date", "URL")
    ))
    dashes <- paste0(rep("----------", times = 7), collapse = "")
    packageStartupMessage(
        paste0(dashes, "\n  ",
               pkg.info["Package"], ": ", pkg.info["Title"], "\n\n  ",
               "Para colabora\u00e7\u00e3o, suporte ou ",
               "relato de bugs, visite:\n    ",
               pkg.info["URL"], "\n\n  ",
               pkg.info["Package"], " version ", pkg.info["Version"],
               " (feito em ", pkg.info["Date"], ") foi carregado.\n",
               dashes)
    )
}
