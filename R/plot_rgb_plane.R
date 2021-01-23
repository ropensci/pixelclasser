#' Plots a triangular plot to be filled with pixels and rules
#'
#' Plots a plane of the two variables selected by the user (\code{r}, \code{g}
#' or \code{b}) and, to serve as visual references, the lines limiting the
#' triangular area that can contain pixels (in blue) and the areas where one of
#' the colour variables has the larger proportion values (in red). Points
#' representing the pixels of a transformed image and lines representing the
#' rules can be later added to the plot using functions \code{plot_pixels()} and
#' \code{plot_rule()}.
#'
#' @param x_axis a character string indicating which colour variable use as x.
#' @param y_axis a character string indicating which colour variable use as y.
#' @param plot_limits a logical value. When TRUE (default) the limits of the
#'   area where the pixels can be found are plotted.
#' @param plot_guides a logical value. When TRUE (default) the limits of the
#'   area where one variable dominates are plotted.
#' @param plot_grid a logical value; if TRUE (default) a grid is added.
#' @param \dots allows passing graphical parameters to the plotting functions.
#'
#' @return The function does not return any value.
#'
#' @details Graphical parameters can be passed to the function to modify the
#'   appearance of the plot. Intended for passing \code{xlim} and \code{ylim}
#'   values to plot the part of the graph where the points are concentrated.
#'   
#'   Because the variables were transformed into proportions, the pixel are
#'   always inside the triangle defined by the points \code{(0, 0), (1, 0), (0,
#'   1)}. This triangle is plotted in blue. The point where all three variables
#'   have the same value is \code{(1/3, 1/3)}. The lines joining this point with
#'   the centers of the triangle sides divide the areas where one of the three
#'   variables has higher proportions than the other two. These lines are
#'   plotted as visual aids, so they can be deleted at will.
#'
#' @seealso \code{\link{plot_pixels}}, \code{\link{plot_rule}},
#'   \code{\link{define_rule}}
#'
#' @examples
#' # Simplest call
#' plot_rgb_plane("g", "b")
#'
#' # Plane without the red lines
#' plot_rgb_plane("g", "b", plot_guides = FALSE)
#'
#' # Restricting the plane area to show
#' plot_rgb_plane("g", "b", xlim = c(0.2, 0.5), ylim = c(0.0, 0.33))
#'
#' @export

plot_rgb_plane <- function(x_axis, y_axis, plot_limits = TRUE,
                           plot_guides = TRUE, plot_grid = TRUE, ...){

  graphics::plot(c(0, 1), c(0, 1), type = "n", asp = 1,
                 xlab = x_axis, ylab = y_axis, ... = ...)

  if(plot_grid){
    graphics::grid()
  }
  if (plot_limits){
    graphics::lines(c(0,0), c(1,0), col="blue")
    graphics::lines(c(0,1), c(0,0), col="blue")
    graphics::lines(c(0,1), c(1,0), col="blue")
  }
  if (plot_guides){
    graphics::lines(c(0, 1/3), c (1/2, 1/3), col="red")
    graphics::lines(c(1/2, 1/3), c (0, 1/3), col="red")
    graphics::lines(c(1/3, 1/2), c (1/3, 1/2), col="red")
  }
}
