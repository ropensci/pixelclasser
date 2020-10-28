#' Plot the pixels of a transformed image
#'
#' This function is a wrapper for function \code{points()} in package
#' \code{graphics} for plotting the pixels of a transformed rgb image on the
#' triangular diagram previously created by \code{plot_rgb_plane()}.
#'
#' @param image_rgb an object produced by \code{transform_colours()}.
#' @param x_axis a character string indicating which colour variable use as x.
#' @param y_axis a character string indicating which colour variable use as y.
#' @param col a character string defining the colour to plot the pixels.
#'
#' @return The function does not return any value.
#'
#' @details It is advantageous to specify a colour such as \code{"#00000005"}
#'   which is black but highly transparent. In this way a density plot is
#'   created because the clustering of points creates areas of darker colour.
#'   Note that a colour without specific transparency information defaults to an
#'   opaque colour, so \code{"#000000"} is the same as \code{"#000000ff"}. The
#'   colours must be specified in any form understandable by function
#'   \code{col2rgb}
#'
#'   Caution: The colour variables passed in \code{x_axis} and \code{y_axis}
#'   must be the same that were used to plot the plane.
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#' \dontrun{
#'
#' # Plotting the pixels of the example image included in this package
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                                        package = "pixelclasser"))
#' plot_rgb_plane("g", "b")
#' plot_pixels(ivy_oak_rgb, "g", "b", "#00000005")
#' }
#'
#' @export

plot_pixels <- function(image_rgb, x_axis, y_axis, col = "black"){

  if (!identical(class(image_rgb), "transformed_image")){
    stop("The image object to plot,", image_rgb,
         "is not of class 'transformed_image.\n",
         call. = FALSE)
  } else {
    graphics::points(image_rgb[,, x_axis], image_rgb[,, y_axis],
                     pch = ".", col = col)
  }
}
