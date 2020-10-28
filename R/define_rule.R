#' Creates a rule object
#'
#' Creates an object of class \code{rule} from a line defined by the user and a
#' relational operator.
#'
#' @param rule_name a character string containing the name of the rule.
#' @param x_axis a character string indicating the colour variable that
#'   corresponds to the x axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string indicating the colour variable that
#'   corresponds to the y axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param first_point a numeric vector containing the \code{(x, y)} coordinates
#'   of the first point.
#' @param second_point a numeric vector containing the \code{(x, y)} coordinates
#'   of the second point.
#' @param comparator a character string containing one of the relational
#'   operators \code{">", ">=", "<", "<="}.
#'
#' @return A list of class \code{rule} containing the following elements:
#'   \itemize{ \item \code{rule_name}: a character string containing the rule
#'   name. \item \code{rule_text}: a character string containing the
#'   mathematical expression of the rule. \item \code{a}: a numerical vector
#'   containing the parameter \code{a} (slope) of the line. \item \code{c}: a
#'   numerical vector containing the parameter \code{c} (intercept) of the line.
#'   \item \code{x_axis}: a character string containing the colour variable
#'   selected as \code{x} axis. \item \code{y_axis}: a character string
#'   containing the colour variable selected as \code{y} axis. \item
#'   \code{first_point}: a numerical vector containing the coordinates of the
#'   first point used to estimate the line equation. \item \code{second_point}:
#'   a numerical vector containing the coordinates of the second point. }
#'
#' @details This function estimates the parameters \code{a} and \code{c} of the
#'   line \code{y = ax + c} from the coordinates of two points on the line.
#'   \code{x} and \code{y} are two colour variables selected by the user
#'   (\code{r}, \code{g}, or \code{b}). The line divides the plane in two
#'   subsets and the operator selects the subset that contains the points
#'   (pixels) of interest.
#'
#'   The pair of points used to define the line must have mathematical sense,
#'   but are not constrained to belong to the area inside the blue triangle.
#'   Note that first_ and second_point are stored in the \code{rule} object and
#'   are used by \code{plot_rule()} as the start and end of the plotted line. If
#'   it is convenient to extend the line outside the triangle, simply use
#'   adequate start and end points. This changes how the line is plotted, not
#'   its equation, so the classification of the pixels does not change.
#'
#'   When the x coordinates of the two points are the same, the line is
#'   vertical. This is an special case which is handled appropriately. The slope
#'   is \code{a = Inf} and the equation is \code{x = c}. The comparator defines
#'   the side of the line were the pixels stand.
#'
#' @seealso \code{\link{define_subcat}}, \code{\link{define_cat}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#' @examples
#' rule01 <- define_rule("rule01", "g", "b", c(0.35, 0.30), c(0.45, 0.10), ">")
#'
#' # A vertical line as a rule; note that the equation is simplified
#' rule02 <- define_rule("rule02", "g", "b", c(0.35, 0.30), c(0.35, 0.00), ">")
#'
#' @export

define_rule <- function(rule_name, x_axis, y_axis,
                        first_point, second_point, comparator){

  x <- 1
  y <- 2

  # Parameter checks -----------------------------------------------------------

  if (!(x_axis %in% c('r', 'g', 'b'))){
    stop('The x_axis must be one of "r", "g" or "b"')
  }
  if (!(y_axis %in% c('r', 'g', 'b'))){
    stop('The y_axis must be one of "r", "g" or "b"')
  }
  if (!(comparator %in% c(">", ">=", "<", "<="))){
    stop('The comparation operator must be one of ">", ">=", "<" or "<="')
  }
  if (identical(x_axis, y_axis)){
    stop('x_axis and y_axis must be different')
  }
  if (identical(first_point, second_point)){
    stop('Start and end points are the same')
  }

  # Line estimation -----------------------------------------------------------
  a <- (second_point[y] - first_point[y]) / (second_point[x] - first_point[x])

  if (is.infinite(a)){
    # A vertical line; formula: x_axis <comparator> c
    c <- first_point[x]
    rule_text <- paste("image_prop[,, ", x_axis, "]",comparator, c)
  } else{
    # Other lines: formula: y_axis <comparator> a * x_axis + c
    c <- first_point[y] - (a * first_point[x])
    rule_text <- paste("image_prop[,, ", y_axis, "]", comparator,
                       a, "*", "image_prop[,,", x_axis, "]", "+", c)
  }
  # Result construction --------------------------------------------------------
  result <- list("rule_name" = rule_name, "rule_text" = rule_text,
                 "a" = a, "c" = c, "x_axis" = x_axis, "y_axis" = y_axis,
                 "first_point" = first_point, "second_point" = second_point)
  class(result) <- c("rule")

  return(result)
}
