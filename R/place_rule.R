#' Places a line on the rgb plot
#'
#' A wrapper function for \code{graphics::locator} that makes the creation
#' of rules easier.
#'
#' @param x_axis a character string indicating the colour variable that
#'   corresponds to the x axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string indicating the colour variable that
#'   corresponds to the y axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param line_type a character string indicating that the line is
#'   \code{"vertical"}, \code{"horizontal"} or otherwise (\code{"free"}, the
#'   default).
#'
#' @return A list of class \code{rule_points} containing the following elements:
#' \itemize{
#' \item \code{x_axis}: a character string containing the colour variable
#'   selected as \code{x} axis.
#' \item \code{y_axis}: a character string containing the colour variable
#'   selected as \code{y} axis.
#' \item \code{first_point}: coordinates of the start point of the line.
#' \item \code{second_point}: coordinates of the end point of the line.
#' }
#' @details This function calls \code{graphics::locator} allowing to select two
#'   points, plots the line joining these points and returns a list
#'   containing their coordinates. The coordinates are rearranged to
#'   pass them to \code{define_rule()}.
#'
#'   True horizontal and vertical lines are difficult to create by hand. In
#'   these cases, specifying \code{"vertical"} or \code{"horizontal"} (partial
#'   match allowed, i e "h") will copy the appropriate coordinate value from the
#'   first point to the second. Note that this is done after \code{locator()}
#'   returns, so the plot will show the line joining the original points, not
#'   the corrected ones. Use \code{plot_rule()} to see corrected line.
#'
#' @seealso \code{\link[graphics]{locator}}, \code{\link{define_rule}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#' @examples
#' \dontrun{
#' plot_rgb_plane("r", "g")
#' line01 <- place_rule("r", "g")          # A "free" line
#' line02 <- place_rule("r", "g", "h")     # A horizontal line}
#' }
#' 
#' @export

place_rule <- function(x_axis, y_axis, line_type = 'free'){

  # Parameter tests ------------------------------------------------------------
  # Standard parameter error is thrown after the selecting the line points, so
  # this test is needed to stop the function early.
  if (missing(x_axis) | missing(y_axis)){
    stop('X or Y colour variable missing', call. = F)
  }
  
  # Main code ------------------------------------------------------------------
  x <- 1
  y <- 2
  
  result <- vector(mode = "list", length = 4)
  names(result) <- c("x_axis", "y_axis", "first_point", "second_point")
  class(result) <- "rule_points"
  
  coordinates <- graphics::locator(n = 2, type = "l")
  result$first_point <- c(coordinates$x[1], coordinates$y[1])
  result$second_point <- c(coordinates$x[2], coordinates$y[2])
  names(result$first_point) <- c("x", "y")
  names(result$second_point) <- c("x", "y")
  result$x_axis = x_axis
  result$y_axis = y_axis
  
  if (startsWith("vertical", line_type)){
    result$second_point["x"] <- result$first_point["x"]
  }
  
  if (startsWith("horizontal", line_type)){
    result$second_point["y"] <- result$first_point["y"]
  }
  
  return(result)
}
