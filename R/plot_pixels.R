#' Plot the pixels of a transformed image
#'
#' This function is a wrapper for function \code{points()} in package
#' \code{graphics} for plotting the pixels of a transformed rgb image on the
#' triangular diagram previously created by \code{plot_rgb_plane()}.
#'
#' @param image_rgb an object produced by \code{read_image()}.
#' @param x_axis a character string indicating which colour variable use as x.
#' @param y_axis a character string indicating which colour variable use as y.
#' @param \dots additional graphical parameters, mainly to set the colour
#'   (\code{col}) of the points.
#'
#' @return The function does not return any value.
#'
#' @details It is advantageous to specify a colour such as \code{"#00000005"}
#'   which is black but almost transparent. In this way a kind of density plot
#'   is created because the clustering of points creates areas of darker colour.
#'   Note that a colour without specific transparency information defaults to an
#'   opaque colour, so \code{"#000000"} is the same as \code{"#000000ff"}. The
#'   colours can be specified in any form understandable by
#'   \code{grDevices::col2rgb}, but the hexadecimal string allows setting the
#'   colour transparency and it is the preferred style. Note also that the
#'   points are plotted using pch = ".", as any other symbol would clutter the
#'   graph.
#'   
#'   Warning: plotting several million points in an R graph is a slow process. 
#'   Be patient or reduce the size of the images as much as possible.
#'   Having a nice smartphone with a petapixel camera sensor is good for
#'   artistic purposes, but not always for efficient scientific work.
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#'
#' # Plotting the pixels of the example image included in this package
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                                        package = "pixelclasser"))
#' plot_rgb_plane("g", "b")
#' plot_pixels(ivy_oak_rgb, "g", "b", col = "#00000005")
#'
#' @export

plot_pixels <- function(image_rgb, x_axis, y_axis, ...){

  if (!identical(class(image_rgb), "transformed_image")){
    stop("The image object to plot,", image_rgb,
         "is not of class 'transformed_image.\n",
         call. = FALSE)
  } else {
    graphics::points(image_rgb[,, x_axis], image_rgb[,, y_axis],
                     pch = ".", ... =  ...)
  }
}
