#' pixelclasser: Functions to classify pixels by colour
#'
#' \code{pixelclasser} contains functions to classify the pixels of an image
#' file (jpeg or tiff) by its colour. It uses a simple form of the technique
#' known as Support Vector Machine, adapted to this particular problem. The
#' original colour variables (\code{R, G, B}) are transformed into colour
#' proportions and the resulting two dimensional plane defined any convenient
#' pair of the transformed variables -- \code{r, g, b}) is divided in several
#' subsets (categories) by one or more straight lines (rules) selected by the
#' user. Finally, the pixels belonging to each category are identified using the
#' rules, and a classified image can be created and saved.
#'
#' @details To classify the pixels of an image, a series of steps must be done
#' in the following order, using the functions in parenthesis:
#' \itemize{
#' \item import the image into an R array of transformed (rgb) data
#' (\code{read_image()}).
#' \item plot the pixels of the image on the plane of two transformed variables
#' (\code{plot_rgb_plane()}, \code{plot_pixels}).
#' \item trace lines between the pixels and use them to create classification
#' rules (\code{place_rule()}, \code{define_rule}, \code{plot_rule()}).
#' \item combine the rules to define categories. Sometimes the rules are
#' combined into subcategories and these into categories (\code{define_cat()}, 
#' \code{define_subcat()}).
#' \item use the categories to classify the pixels (\code{classify_pixels()}).
#' \item save the results of the classification as an image
#' (\code{save_clasif_image()}).
#' }
#' 
#' These steps are explained in depth in the vignette included in the package.
#'
#' @author Carlos Real (carlos.real@usc.es)
#'
#' @docType package
#' @name pixelclasser
#'
NULL
