
#' @title Escape accented characters in a document
#'
#' @description This function is used by the "Escape Unicode" add-in
#' and removes all accented characters from the current file, replacing
#' them by their equivalent Unicode-escaped values.
#'
#' @export
escape_unicode <- function() {

  # Escape accented characters from one character vector
  escape <- . %>%
    iconv("UTF-8", "ASCII", sub = "Unicode") %>%
    stringr::str_replace_all("<U\\+([0-9A-F]+)>", "\\\\u\\1")

  # Escape accented characters from every line of the document
  escape_all <- . %>%
    purrr::map_chr(escape) %>%
    stringr::str_c(collapse = "\n")

  # Get text of document and escape accented characters
  doc <- rstudioapi::getSourceEditorContext()

  # Change behavior if selection exists
  text <- purrr::pluck(doc, "selection", 1, "text")
  if (stringr::str_length(text) > 0) {
    sel <- stringr::str_split(text, "\n")[[1]]
    rstudioapi::insertText(purrr::pluck(doc, "selection", 1, "range"), escape_all(sel))
  } else {
    rstudioapi::setDocumentContents(escape_all(doc$contents), doc$id)
  }
}
