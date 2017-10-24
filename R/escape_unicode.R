
#' Escape accented characters in a document
#' 
#' @export
escape_unicode <- function() {
  
  # Escape accented characters from one character vector
  escape <- . %>%
    stringi::stri_escape_unicode() %>%
    stringi::stri_replace_all_fixed("\\\\", "#$!*%") %>%
    stringi::stri_replace_all_fixed("\\", "") %>%
    stringi::stri_replace_all_fixed("#$!*%", "\\") %>%
    stringr::str_replace_all("(u00[:alnum:]{2})", "\\\\\\1")
  
  # Escape accented characters from every line of the document
  escape_all <- . %>%
    purrr::map_chr(escape) %>%
    stringr::str_c(collapse = "\n")
  
  # Get text of document and escape accented characters
  doc <- rstudioapi::getSourceEditorContext()
  rstudioapi::setDocumentContents(escape_all(doc$contents), doc$id)
}
