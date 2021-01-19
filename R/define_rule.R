#' Creates a rule object
#'
#' Creates an object of class \code{pixel_rule} from a line defined by the user
#' and a relational operator.
#'
#' @param rule_name a character string containing the name of the rule.
#' @param x_axis a character string indicating the colour variable that
#'   corresponds to the x axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string indicating the colour variable that
#'   corresponds to the y axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param rule_points an object of  of class \code{"rule_points"} created with
#'   function \code{place_rule()}, or a list containing the coordinates of two
#'   points that define the line.
#' @param comp.op a character string containing one of the comparison
#'   operators \code{">", ">=", "<", "<="}.
#'
#' @return A list of class \code{pixel_rule} containing the following elements:
#'   \itemize{ \item \code{rule_name}: a character string containing the rule
#'   name. \item \code{rule_text}: a character string containing the
#'   mathematical expression of the rule. \item \code{comp.op}: a character
#'   string containing the comparison operator used in the rule. \item \code{a}:
#'   a numerical vector containing the parameter \code{a} (slope) of the line.
#'   \item \code{c}: a numerical vector containing the parameter \code{c}
#'   (intercept) of the line. \item \code{x_axis}: a character string containing
#'   the colour variable selected as \code{x} axis. \item \code{y_axis}: a
#'   character string containing the colour variable selected as \code{y} axis.
#'   \item \code{first_point}: a numerical vector containing the coordinates of
#'   the first point used to estimate the line equation. \item
#'   \code{second_point}: a numerical vector containing the coordinates of the
#'   second point. }
#'
#' @details This function estimates the parameters \code{a} and \code{c} of the
#'   line \code{y = ax + c} from the coordinates of two points on the line.
#'   \code{x} and \code{y} are two colour variables selected by the user
#'   (\code{r}, \code{g}, or \code{b}). The line divides the plane in two
#'   subsets and the operator selects the subset that contains the points
#'   (pixels) of interest.
#'   
#'   When \code{rule_points} is a list, it is internally converted into an
#'   an object of type \code{rule_points}.
#'
#'   The pair of points used to define the line must have mathematical sense,
#'   but are not constrained to belong to the area inside the blue triangle.
#'   Note that the coordinates of the points are stored in the \code{pixel_rule}
#'   object and are used by \code{plot_rule()} as the start and end of the
#'   plotted line. If it is convenient to extend the line outside the triangle,
#'   simply use adequate start and end points. This changes how the line is
#'   plotted, not its equation, so the classification of the pixels does not
#'   change.
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
#' rule_points01 <- place_rule()
#' rule01 <- define_rule("rule01", "g", "b", rule_points01,">")
#' 
#' # Note that the creation of the intermediate object can be avoided:
#' rule01 <- define_rule("rule01", "g", "b", place_rule(),">")
#' }
#' @export

define_rule <- function(rule_name, x_axis, y_axis, rule_points, comp.op){

  x <- 1
  y <- 2

  if (identical(class(rule_points), "list")){
    if (length(rule_points) != 2){
      stop('rule_points must contain two points')
    }
    names(rule_points) <- c("first_point", "second_point")
    class(rule_points) <- "rule_points"
  } else {
    stop('rule_points must be a list containing two coordinate vectors')
  }
  
  # Parameter checks -----------------------------------------------------------

  if (!(x_axis %in% c('r', 'g', 'b'))){
    stop('The x_axis must be one of "r", "g" or "b"')
  }
  if (!(y_axis %in% c('r', 'g', 'b'))){
    stop('The y_axis must be one of "r", "g" or "b"')
  }
  if (!(comp.op %in% c(">", ">=", "<", "<="))){
    stop('The comparation operator must be one of ">", ">=", "<" or "<="')
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
    # A vertical line; formula: x_axis <comp.op> c
    c <- rule_points$first_point[x]
    rule_text <- paste("image_prop[,, ", x_axis, "]",comp.op, c)
  } else{
    # Other lines: formula: y_axis <comp.op> a * x_axis + c
    c <- rule_points$first_point[y] - (a * rule_points$first_point[x])
    rule_text <- paste("image_prop[,, ", y_axis, "]", comp.op,
                       a, "*", "image_prop[,,", x_axis, "]", "+", c)
  }
  # Result construction --------------------------------------------------------
  result <- list("rule_name" = rule_name, "rule_text" = rule_text,
                 "comp.op" = comp.op, "a" = a, "c" = c, "x_axis" = x_axis,
                 "y_axis" = y_axis, "first_point" = rule_points$first_point,
                 "second_point" = rule_points$second_point)
  class(result) <- c("pixel_rule")

  return(result)
}
