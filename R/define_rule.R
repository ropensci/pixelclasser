#' Creates a rule object
#'
#' Creates an object of class \code{pixel_rule} from a line in \code{rgb} space,
#' defined by the user, and a relational operator.
#'
#' @param rule_name a character string containing the name of the rule.
#' @param x_axis a character string selecting the colour variable used as x
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string selecting the colour variable used as y
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param rule_points either an object of  of class \code{"rule_points"} created
#'   with function \code{place_rule()}, or a list containing the coordinates of
#'   two points defining the line.
#' @param comp_op a character string containing one of the comparison operators
#'   \code{">", ">=", "<", "<="}.
#'
#' @return A list of class \code{pixel_rule} containing the following elements:
#'   \itemize{
#'   \item \code{rule_name}: a character string containing the rule name.
#'   \item \code{rule_text}: a character string containing the mathematical
#'   expression of the rule.
#'   \item \code{comp_op}: a character string containing the comparison operator
#'   used in the rule.
#'   \item \code{a}: a numerical vector containing the parameter \code{a}
#'   (slope) of the line.
#'   \item \code{c}: a numerical vector containing the parameter \code{c}
#'   (intercept) of the line.
#'   \item \code{x_axis}: a character string containing the colour variable
#'   selected as \code{x} axis.
#'   \item \code{y_axis}: a character string containing the colour variable
#'   selected as \code{y} axis.
#'   \item \code{first_point}: a numerical vector containing the coordinates of
#'   the first point used to estimate the line equation.
#'   \item \code{second_point}: a numerical vector containing the coordinates of
#'   the second point.
#'   }
#'
#' @details This function estimates the slope (\code{a}) and intercept
#'   (\code{c}) of the line \code{y = ax + c} using the coordinates of two
#'   points on the line. \code{x} and \code{y} are two colour variables selected
#'   by the user (\code{r}, \code{g}, or \code{b}). The line divides the plane
#'   in two subsets and the comparison operator selects the subset that contains
#'   the points (pixels) of interest.
#'
#'   When a list of two points is passed in \code{rule_points}, it is internally
#'   converted into an an object of class \code{rule_points}.
#'
#'   The pair of points used to define the line are not constrained to belong to
#'   the area occupied by the pixels, but they are used by \code{plot_rule()} as
#'   the start and end of the plotted line. Therefore, the extremes of the line
#'   can be selected in the most convenient way, provided that the line divides
#'   correctly the categories. Convenience means that the line should seem nice
#'   in the plot, if this matters.
#'   
#'   Because the variables were transformed into proportions, the pixel are
#'   always inside the triangle defined by the points \code{(0, 0), (1, 0), (0,
#'   1)}. So, the sides of this triangle can be considered as implicit rules
#'   which do not need to be created. In this way, a single line creates two
#'   polygons by cutting the triangle in two. The implicit rules can reduce the
#'   number of rules to create in most cases.
#'
#' @seealso \code{\link{define_subcat}}, \code{\link{define_cat}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#' @examples
#' # Creating the line by passing the coordinates of two points on the line:
#' rule01 <- define_rule("rule01", "g", "b",
#'                       list(c(0.35, 0.30), c(0.45, 0.10)),">")
#'
#' # A vertical line as a rule; note that the equation is simplified
#' rule02 <- define_rule("rule02", "g", "b",
#'                       list(c(0.35, 0.30), c(0.35, 0.00)), ">")
#' \dontrun{
#' # Creating the rule by passing an object of type rule_point:
#' rule_points01 <- place_rule("g", "b")
#' rule03 <- define_rule("rule03", "g", "b", rule_points01,">")
#'
#' # Note that the creation of the intermediate object can be avoided:
#' rule04 <- define_rule("rule04", "g", "b", place_rule("g", "b"),">")
#' }
#' @export

define_rule <- function(rule_name, x_axis, y_axis, rule_points, comp_op){

  x <- 1
  y <- 2

  if (identical(class(rule_points), "list")){
    if (length(rule_points) != 2){
      stop('rule_points must contain two points', call. = FALSE)
    }
    names(rule_points) <- c("first_point", "second_point")
    class(rule_points) <- "rule_points"
  } else {
    if (!identical(class(rule_points), "rule_points")){
      stop('rule points must contain a list or a rule_point object',
           call. = FALSE)
    }
    if (!(identical(x_axis, rule_points$x_axis) & 
          identical(y_axis, rule_points$y_axis))){
      stop('rule_points axis are not the same as x_axis and y_axis',
           .call = FALSE)
    }
  }
  
  # Parameter checks -----------------------------------------------------------

  if (!(x_axis %in% c('r', 'g', 'b'))){
    stop('The x_axis must be one of "r", "g" or "b"', call. = FALSE)
  }
  if (!(y_axis %in% c('r', 'g', 'b'))){
    stop('The y_axis must be one of "r", "g" or "b"', call. = FALSE)
  }
  if (!(comp_op %in% c(">", ">=", "<", "<="))){
    stop('The comparation operator must be one of ">", ">=", "<" or "<="',
         call. = FALSE)
  }
  if (identical(x_axis, y_axis)){
    stop('x_axis and y_axis must be different')
  }
  if (identical(rule_points$first_point, rule_points$second_point)){
    stop('Start and end points are the same')
  }

  # Line estimation ------------------------------------------------------------
  a <- (rule_points$second_point[y] - rule_points$first_point[y]) /
       (rule_points$second_point[x] - rule_points$first_point[x])

  if (is.infinite(a)){
    # A vertical line; formula: x_axis <comp_op> c
    c <- rule_points$first_point[x]
    rule_text <- paste("image_prop[,, ", x_axis, "]",comp_op, c)
  } else{
    # Other lines: formula: y_axis <comp_op> a * x_axis + c
    c <- rule_points$first_point[y] - (a * rule_points$first_point[x])
    rule_text <- paste("image_prop[,, ", y_axis, "]", comp_op,
                       a, "*", "image_prop[,,", x_axis, "]", "+", c)
  }
  # Result construction --------------------------------------------------------
  result <- list("rule_name" = rule_name, "rule_text" = rule_text,
                 "comp_op" = comp_op, "a" = a, "c" = c, "x_axis" = x_axis,
                 "y_axis" = y_axis, "first_point" = rule_points$first_point,
                 "second_point" = rule_points$second_point)
  class(result) <- c("pixel_rule")

  return(result)
}
