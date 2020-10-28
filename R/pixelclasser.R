#' pixelclasser: Functions to classify pixels by colour
#'
#' \code{pixelclasser} contains a functions to classify the pixels of an image
#' file (jpeg or tiff) by its colour. It uses a simple form of the technique
#' known as Support Vector Machine, adapted to this particular problem. The
#' original colour variables (\code{R, G, B}) are transformed into colour
#' proportions and the resulting two dimensional plane defined any convenient
#' pair of the transformed variables -- \code{r, g, b}) is divided in several
#' subsets (categories) by one or more straight lines (rules) selected by the
#' user. Finally, the pixels belonging to each category are identified using the
#' rules, and a classified image can be created and saved.
#'
#' @details There are four types of functions to:
#' \itemize{
#' \item import the image into an R array and save the classified image.
#' \item create the objects containing the classification rules.
#' \item classify the pixels using the rules.
#' \item plot the pixels and the rules in appropriate graphs.
#' }
#'
#' @author Carlos Real (carlos.real@usc.es)
#'
#' @docType package
#' @name pixelclasser
#'
NULL
