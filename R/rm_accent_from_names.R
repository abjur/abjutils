#' Remove accentuation from column names
#' 
#' Remove accented characters from column names converting them to ASCII.
#' 
#' @param dat A dataset
#' 
#' @return A version of dat without non-ASCII characters.
#' 
#' @export
rm_accent_from_names <- function(dat){
  old_names <- names(dat)
  new_names <- rm_accent(old_names)
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1], dupe_count[dupe_count > 1], sep = "_")
  stats::setNames(dat, new_names)
}