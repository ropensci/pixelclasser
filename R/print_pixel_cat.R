#' @export

print.pixel_cat <- function(x, digits = NULL, quote = TRUE,
                            na.print = NULL, print.gap = NULL, right = FALSE,
                            max = NULL, useSource = TRUE, ...){
  cat('\nCategory name: "', x$name, '"\n', sep = '')
  cat('Colour (as RGB [0:255] values): (',
      round(x$colour[1,1]*255), ', ', 
      round(x$colour[2,1]*255), ', ',
      round(x$colour[3,1]*255), ')\n',
      sep = '')
  cat("Subcategories:\n\n")
  for (i in seq_along(x$subcats)){
    print(x$subcats[i])
  }
}
