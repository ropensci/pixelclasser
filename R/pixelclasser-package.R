#' pixelclasser: Functions to classify pixels by colour
#'
#' \code{pixelclasser} contains functions to classify the pixels of an image
#' file (in format jpeg or tiff) by its colour. It uses a simple form of the
#' technique known as Support Vector Machine, adapted to this particular
#' problem. The original colour variables (\code{R, G, B}) are transformed into
#' colour proportions (\code{r, g, b}), and the resulting two dimensional plane,
#' defined by any convenient pair of the transformed variables is divided in
#' several subsets (categories) by one or more straight lines (rules) selected
#' by the user. Finally, the pixels belonging to each category are identified
#' using the rules, and a classified image can be created and saved.
#'
#' @details To classify the pixels of an image, a series of steps must be done
#' in the following order, using the functions shown in parenthesis:
#' \itemize{
#' \item import the image into an R array of transformed (\code{rgb}) data
#' (\code{read_image()}).
#' \item plot the pixels of the image on the plane of two transformed variables
#' that shows the categories of pixels most clearly (\code{plot_rgb_plane()},
#' \code{plot_pixels}).
#' \item trace lines between the pixel clusters and use them to create
#' classification rules (\code{place_rule()}, \code{define_rule},
#' \code{plot_rule()}).
#' \item combine the rules to define categories. Sometimes the rules are
#' combined into subcategories and these into categories (\code{define_cat()}, 
#' \code{define_subcat()}).
#' \item use the categories to classify the pixels (\code{classify_pixels()}).
#' \item save the results of the classification as an image, if needed
#' (\code{save_clasif_image()}).
#' }
#' 
#' These steps are explained in depth in the vignette included in the package.
#'
#' @author Carlos Real (carlos.real@usc.es)
#'
#' @docType package
#' @name pixelclasser
#' @import jpeg tiff
#'
NULL
