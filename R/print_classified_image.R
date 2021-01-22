#' @export

print.classified_image <- function(x, digits = NULL, quote = TRUE,
                                   na.print = NULL, print.gap = NULL,
                                   right = FALSE, max = NULL, useSource = TRUE,
                                   ...){
  for (item in seq(1, length(x)- 1)){
    cat('\nCategory name: "', names(x)[item], '"\n')
    cat('Colour (as RGB [0:255] values): (',
        round(x[[item]]$colour[1,1]*255), ', ', 
        round(x[[item]]$colour[2,1]*255), ', ',
        round(x[[item]]$colour[3,1]*255), ')\n',
        sep = '')
    cat(x$summary[[item]], '\n')
  }
  cat(x$summary[[length(x)]], '\n')
}