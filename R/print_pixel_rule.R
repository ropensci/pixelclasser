#' @export

print.pixel_rule <- function(x, digits = NULL, quote = TRUE,
                             na.print = NULL, print.gap = NULL, right = FALSE,
                             max = NULL, useSource = TRUE, ...){
  cat('Rule name: "', x$rule_name, '"\n', sep = '')
  if (x$c < 0){
    cat("Line equation: ", x$y_axis, ' ', x$comp_op, ' ', x$a, ' ', x$x_axis,
        ' - ', abs(x$c), '\n', sep = '')
  } else {
    cat("Line equation: ", x$y_axis, ' ', x$comp_op, ' ', x$a, ' ', x$x_axis,
        ' + ', x$c, '\n', sep = '')
  }
  cat("Point coordinates: (", x$first_point[1], ', ', x$first_point[2], '); (',
      x$second_point[1], ', ', x$second_point[2], ')', sep =  '')
}
