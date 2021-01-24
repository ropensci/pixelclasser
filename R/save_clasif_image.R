#' Saves a classified image in TIFF or JPEG format
#'
#' Creates an image file in TIFF or JPEG format from an array of class
#' \code{classified_image}.
#'
#' @param classified_image an object of class \code{classified_image}.
#' @param file_name a character string with the name of the output file,
#'   including the extension.
#' @param ... further parameters to pass to functions \code{writeJPG} and
#'   \code{writeTIFF}. If void, the default values of these functions are used.
#'
#' @return It does not return anything, only creates the file.
#'
#' @details The type of the output file (jpeg or tiff) is selected from the
#'   extension included in the file name. It must be one of \code{("jpg", "JPG",
#'   "jpeg", "JPEG", "tif", "TIF", "tiff", "TIFF")}.
#'
#'   Note that the default value for jpg quality is 0.7. For maximal quality set
#'   \code{quality = 1} using the \dots argument. Such adjustments are not
#'   needed with \code{tiff} files, as this is a lossless format.
#'
#' @seealso \code{\link{classify_pixels}}
#'
#'   For more information about the options for file formatting see see the help
#'   pages of \code{\link[jpeg]{readJPEG}} and \code{\link[tiff]{readTIFF}}
#'   functions in packages \code{jpeg} and \code{tiff}, respectively.
#'
#' @examples
#' \dontrun{
#'
#' # Saving an hypothetical image. Note the use of quality to set the
#' # maximum quality level in the JPEG file
#' save_classif_image(image01_class, "./myimages/image01_classified.jpg",
#'                    quality = 1)
#' }
#'
#' @export

save_classif_image <- function(classified_image, file_name, ...){

  # Definitions of the band (= colour variables) to make the code more explicit
  r <- 1
  g <- 2
  b <- 3

  bands <- c(r, g, b)

  file_name_elements <- unlist(strsplit(file_name, "[.]"))

  if (!(file_name_elements[length(file_name_elements)] %in%
        c("jpg", "JPG", "jpeg", "JPEG", "tif", "TIF", "tiff", "TIFF"))){
    stop("The extension of ", file_name, " is not 'jpg', 'tif' or equivalent 
         extension.", call. = FALSE)
  }

  image_array <- array(dim = c(dim(classified_image[[1]]$incid_mat), 3), data=0)
  for (band in bands){
    for (pixel_cat in (1:(length(classified_image) - 1))){
      image_array[,, band] <- image_array[,, band] +
                              classified_image[[pixel_cat]]$incid_mat * 
                              classified_image[[pixel_cat]]$colour[band, 1]
    }
  }
  if (file_name_elements[length(file_name_elements)] %in%
      c("tif", "TIF", "tiff", "TIFF")){
    tiff::writeTIFF(image_array, file_name, ... = ...)
  }
  if (file_name_elements[length(file_name_elements)]
      %in% c("jpg", "JPG", "jpeg", "JPEG")){
    jpeg::writeJPEG(image_array, file_name, ... = ...)
  }
}
