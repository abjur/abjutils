
#' @title Gather subjects from esaj::cjsg_table("subjects")
#'
#' @description Once you run `esaj::cjsg_table("subjects")`, you can
#' use this function to gather the subjects automatically. Download
#' `esaj` by running `devtools::install_github("courtsbr/esaj")`.
#'
#' @param subjects Table returned by `esaj::cjsg_table("subjects")`
#'
#' @export
gather_subjects <- function(subjects) {
  unite_index <- function(d, i) {
    tidyr::unite_(d, paste0("level", i), paste0(c("id", "name"), i))
  }
  purrr::reduce(0:5, unite_index, .init = subjects) %>%
    tidyr::gather() %>%
    tidyr::separate(value, c("id", "nm"), sep = "_") %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(id))
}
