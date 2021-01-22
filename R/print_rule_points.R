#' @export

print.rule_points <- function(x, digits = NULL, quote = TRUE,
                             na.print = NULL, print.gap = NULL, right = FALSE,
                             max = NULL, useSource = TRUE, ...){
  
  cat('Colour variables: "', x$x_axis, '", "', x$y_axis, '"\n', sep = '')
  cat('First point: (', x$first_point['x'],', ', x$first_point['y'], ')\n',
      sep = '')
  cat('Second point: (', x$second_point['x'],', ', x$second_point['y'], ')\n',
      sep = '')
}
