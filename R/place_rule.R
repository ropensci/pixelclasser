#' Places a line on the rgb plot
#'
#' A wrapper function for \code{graphics:locator} that facilitates the creation
#' of rules.
#'
#' @param line_type a character string indicating that the line is
#'   \code{"vertical"} or \code{"horizontal"}.
#'
#' @return A list containing the following elements: \itemize{ \item
#'   \code{first_point}: coordinates of the start point of the line. \item
#'   \code{second_point}: coordinates of the end point of the line. }
#'
#' @details This function calls \code{graphics::locator} allowing to select two
#'   points, then plots the line joining these points and returns a list
#'   containing the coordinates of the points. The coordinates are rearranged in
#'   a way convenient to be passed to \code{define_rule()}.
#'
#'   True horizontal and vertical lines are difficult to create by hand. In 
#'   these cases, specifying \code{"vertical"} or \code{"horizontal"} (partial
#'   match allowed, i e "h") will copy the appropriate coordinate value from the
#'   first point to the second. Note that this is done after \code{locator()}
#'   returns, so the plot will show the line as was created by the user, not the
#'   corrected one. Use \code{plot_rule()} to see this.
#'
#' @seealso \code{\link[graphics]{locator}}, \code{\link{define_rule}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#' @examples
#' \dontrun{
#' After creating a plot with the points, the lines are created with
#' line01 <- place_rule()        # A "normal" line
#' line02 <- place_rule("h")     # An horizontal line}
#'
#' @export

place_rule <- function(line_type = 'normal'){

  x <- 1
  y <- 2
  
  result <- vector(mode = "list", length = 2)
  names(result) <- c('first_point', 'second_point')
  
  coordinates <- graphics::locator(n = 2, type = 'l')
  
  result$first_point <- c(coordinates$x[1], coordinates$y[1])
  result$second_point <- c(coordinates$x[2], coordinates$y[2])
  names(result$first_point) <- c('x', 'y')
  names(result$second_point) <- c('x', 'y')
  
  if (startsWith('vertical', line_type)){
    result$second_point['x'] <- result$first_point['x']
  }
  
  if (startsWith('horizontal', line_type)){
    result$second_point['y'] <- result$first_point['y']
  }
  class(result) <- "rule_points"
  return(result)
}
