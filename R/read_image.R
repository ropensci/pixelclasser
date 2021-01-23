#' Imports a jpg or tiff file.
#'
#' Imports an image file (in JPEG or TIFF format) into an array, and converts
#' the original \code{R}, \code{G} and \code{B} values in the file into
#' proportions (\code{r}, \code{g} and \code{b} variables).
#'
#' @param file_name A character string containing the name of the image file.
#'
#' @return Returns an array of dimensions \code{r x c x 3} and class
#'   \code{transformed_image}, being \code{r} and \code{c} the number of rows
#'   and columns in the image. The last dimension corresponds to the \code{R},
#'   \code{G} and \code{B} variables (= bands) that define the colours of the
#'   pixels. The values in the array are the proportions
#'   of each colour (\code{r, g, b}), i.e. \code{r} = \code{R} / (\code{R + G +
#'   B}), and so on.
#'
#' @details This function calls the functions \code{readJPEG()} and
#'   \code{readTIFF()} in packages \code{jpeg} and \code{tiff} to import the
#'   data into an R array. Then it transforms the data into proportions
#'
#' @seealso For more information about jpeg and tiff file formats, see the help
#'   pages of \code{\link[jpeg]{readJPEG}} and \code{\link[tiff]{readTIFF}}
#'   functions in packages \code{jpeg} and \code{tiff}, respectively.
#'
#' @examples
#'
#' # An example that loads the example file included in the package
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG", 
#'                                        package = "pixelclasser"))
#'
#' @export

read_image <- function(file_name){

  file_name_elements <- unlist(strsplit(file_name, "[.]"))
  if (file_name_elements[length(file_name_elements)] %in%
      c("jpg", "JPG", "jpeg", "JPEG")){
    result <- jpeg::readJPEG(file_name)
  } else {
    if (file_name_elements[length(file_name_elements)] %in%
        c("tif", "TIF", "tiff", "TIFF")){
      result <- tiff::readTIFF(file_name)
    } else {
      stop("The extension of ", file_name, " is not jpg, tif or equivalent")
    }
  }

  result <- transform_colours(result)
  class(result) <- c("transformed_image")

  return(result)
}
