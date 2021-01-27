#' Transforms RGB values into proportions (rgb values)
#'
#' This function transforms an array of RGB absolute values into a similar array
#' containing the proportion of each band (= colour variable): r g and b.
#'
#' @param image_array an array of class \code{image_array} created by function
#'   \code{read_image()}.
#'
#' @return Returns an array of class \code{transformed_image} containing the
#'   proportions of each colour variable in the pixels of the image. The third
#'   dimension of the array is named "bands" and its elements are labelled as
#'   "r", "g" and "b", respectively.
#'
#' @details The proportions are calculated as \code{r} = \code{R} / (\code{R + G
#'   + B}), and so on. It is used by function read_image().
#'
#' @keywords internal

transform_colours <- function(image_array){

  # Band names in the original image, to avoid magic numbers in code
  R <- 1
  G <- 2
  B <- 3

  result <- image_array

  image_panchromatic <- image_array[, , R] +
                        image_array[, , G] +
                        image_array[, , B]
  # A small value is added to avoid divide by zero
  image_panchromatic[image_panchromatic == 0] <- 0.00001 
  result[, , R] <- image_array[, , R]/image_panchromatic
  result[, , G] <- image_array[, , G]/image_panchromatic
  result[, , B] <- image_array[, , B]/image_panchromatic
  dimnames(result) <- list("rows" = NULL, "cols" = NULL,
                           "bands" = c("r", "g", "b"))
  class(result) <- "transformed_image"

  return(result)
}
