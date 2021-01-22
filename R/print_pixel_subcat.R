#' @export


print.pixel_subcat <- function(x, digits = NULL, quote = TRUE,
                               na.print = NULL, print.gap = NULL, right = FALSE,
                               max = NULL, useSource = TRUE, ...){
  cat('Subcategory name: "', x$name, '"\n')
  cat("Rules:\n")
  for (i in seq_along(x$rules)){
    print(x$rules[i])
  }
}
