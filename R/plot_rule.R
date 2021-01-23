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
#' @details The function uses the information stored in the \code{pixel_rule
#'   object} to plot the line.
#'
#'   Use the \dots to set the colour and other characteristics of the line. Use
#'   any character string understood by \code{col2rgb()}.
#'   
#'   Labels can be added to the rule using \code{label_rule()}.
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link{define_rule}},
#'   \code{\link{label_rule}} \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#' rule_01 <- define_rule("rule_01", "g", "b",
#'                       list(c(0.345, 1/3), c(0.40, 0.10)), "<")
#'
#' plot_rgb_plane("g", "b")
#' plot_rule(rule_01, col = "green")
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
