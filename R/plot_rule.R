#' Plots the line that defines a rule
#'
#' This function draws the line that defines a rule on the plot created by
#' \code{plot_rgb_plane()}.
#'
#' @param rule an object of class \code{pixel_rule} produced by
#'   \code{define_rule()}.
#' @param label a string to label the line. It is attached at the coordinates of
#'   the second point used to define the line.
#' @param \dots additional graphical parameters passed to the underlying
#'   \code{lines()} function, for example to define the line colour or dashing
#'   style. They are also used for the line label.
#'
#' @return The function does not return any value.
#'
#' @details The function uses the information stored in the pixel_rule object to
#'   plot the line. Therefore the line starts and ends at the points stored in
#'   the pixel_rule object. If these points were not convenient for plotting,
#'   the coordinates of other points on the line can be calculated using its
#'   equation (the pixel_rule object contains the parameters of the line) and
#'   the original coordinates replaced by these.
#'
#'   Use the \dots to set the colour and other characteristics of the line.
#'   These will be applied to the label as well. To apply a different colour to 
#'   the label, do not set a value for this parameter and create the label using
#'   \code{graphics::text()}. To place the label at one end of the line, pass
#'   the coordinates contained in the pixel_rule object to \code{text()}.
#'   
#'   Caution: The colour variables used to define the rules and those used to
#'   plot the triangular graph must be the same.
#'
#'   To set the colour of the line, use any character string understood by
#'   \code{col2rgb()}.
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link{define_rule}},
#'   \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#' rule_01 <- define_rule("rule_01", "g", "b",
#'                       list(c(0.345, 1/3), c(0.40, 0.10)), "<")
#' rule_02 <- define_rule("rule_02","g", "b",
#'                       list(c(0.35, 0.30), c(0.565, 0.10)), "<")
#'
#' plot_rgb_plane("g", "b")
#'
#' # The first rule is represented as a green line without label
#' plot_rule(rule_01, col = "green")
#'
#' # And the second in black with a label (shifted to the right)
#' plot_rule(rule_02, label = expression('L'[1]*''), lty = 1,
#'           col = 'black', adj = 0)
#'
#' @export

plot_rule <- function(rule, label = '', ...){

  if (!identical(class(rule), "pixel_rule")){
    stop("The object to plot must be of class 'pixel_rule'", call. = FALSE)
  }

  x <- numeric(2)
  y <- numeric(2)

  x[1] <- rule$first_point[1]
  x[2] <- rule$second_point[1]
  y[1] <- rule$first_point[2]
  y[2] <- rule$second_point[2]
  graphics::lines(x, y, ...)

}
