#' @title Custom format for NATA reports, based on \code{bookdown::pdf_book}
#' @param keep_tex Whether or not to keep .tex file. Default \code{FALSE}
#' @param latex_engine Engine used to compile pdf. Default \code{'pdflatex'}
#' @param toc Whether or not to show table of contents. Default \code{FALSE}
#' @param toc_depth Depth of levels to show in toc \code{2}
#' @param number_sections Whether or not to number chapters, sections, etc. Default \code{FALSE}
#' @param extra_pandoc_args Extra arguments for pandoc e.g
#' \code{c(
#'   rmarkdown::pandoc_variable_arg("logo_img", "example.png"),
#'   rmarkdown::pandoc_metadata_arg("title","example")
#' )}
#' @param ... Passed on to \code{bookdown::pdf_book(...)}
#' @export
pdf_report = function(keep_tex = TRUE,
                      latex_engine = "xelatex",
                      toc = FALSE,
                      toc_depth = 2,
                      number_sections = FALSE,
                      extra_pandoc_args = NULL,
                      ...) {

  # Locate template
  template = pkg_resource("template.tex")

  # Locate path to fonts
  font_path = system.file("rmarkdown", "templates",
                          "nata-report", "resources/",
                          package = "diagramNAT")
  tna_logo = pkg_resource("tna-square-white-logo.pdf_tex")

  pandoc_args = c(
    rmarkdown::pandoc_variable_arg("font_path", font_path),
    rmarkdown::pandoc_variable_arg("tna_logo", tna_logo),
    extra_pandoc_args
    )

  bookdown::pdf_book(
    template = template,
    keep_tex = keep_tex,
    toc = toc,
    toc_depth = toc_depth,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    number_sections = number_sections
  )
}

pkg_resource = function(file) {
  return(system.file("rmarkdown", "templates",
                     "nata-report", "resources",
                     file, package = "diagramNAT"))
}
