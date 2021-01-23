#' Creates a category object
#'
#' Creates an object of class \code{pixel_cat}, which contains a list of objects
#' of class \code{pixel_subcat}.
#'
#' @param cat_name a character string containing the name of the category.
#' @param cat_colour a character string defining the colour to paint the pixels
#'   with when creating a classified picture.
#' @param \dots a list of \code{pixel_subcat} objects, or \code{pixel_rule}
#'   objects in case that subcategories are not needed. A mixed list of
#'   \code{pixel_rule} and \code{pixel_subcat} objects is not allowed.
#'
#' @return A list of class \code{pixel_cat} with the following elements:
#'   \itemize{ \item \code{name}: a character string containing the name of the
#'   pixel category. \item \code{colour}: a character string describing the
#'   colour of the pixels of the category in the classified images. \item
#'   \code{subcats}: a list containing the subcategories. Their names are the
#'   names of the elements of the list. }
#'
#' @details The function receives a list of objects of class \code{pixel_subcat}
#'   and creates a list of class \code{pixel_cat} with them. However, for cases
#'   that does not need subcategories, i e that only need a set of rules,need a
#'   single set of rules, these can be passed to the function, which creates an
#'   internal subcategory object to contain them. See the examples below.
#'
#'   Note that it is an error to pass a mixture of \code{pixel_rule} and
#'   \code{pixel_subcat} objects.
#'
#'   \code{colour} can be specified in any form understood by
#'   \code{grDevices::col2grb}.
#'
#' @seealso \code{\link{define_rule}}, \code{\link{define_subcat}},
#'   \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#' # The rules are not consistent, they are only useful as examples
#' rule01 <- define_rule("R01", "g", "b",
#'                       list(c(0.35, 0.30), c(0.45, 0.10)), ">=")
#' rule02 <- define_rule("R02", "g", "b",
#'                       list(c(0.35, 0.253), c(0.45, 0.253)), ">=")
#' rule03 <- define_rule("R03", "g", "b",
#'                       list(c(0.35, 0.29), c(0.49, 0.178)), ">=")
#' rule04 <- define_rule("R04", "g", "b",
#'                       list(c(0.35, 0.253), c(0.45, 0.253)), "<")
#'
#' subcat01 <- define_subcat("Subcat01", rule01, rule02)
#' subcat02 <- define_subcat("Subcat02", rule03, rule04)
#'
#' cat01 <- define_cat("Cat01", "#ffae2a", subcat01, subcat02)
#'
#' # A single category defined by a set of rules, not subcategories
#' cat02 <- define_cat("Cat02", "#00ae2a", rule01, rule02, rule03)
#'
#' @export

define_cat <- function(cat_name, cat_colour, ...){

  object_list <- list(...)
  object_names <- vapply(match.call(expand.dots = FALSE)$..., deparse,
                         FUN.VALUE = 'vector')

  # Checking the categories of the objects -------------------------------------
  number_of_subcats <- 0
  number_of_rules <- 0

  for (i in seq_along(object_list)){
    if (identical(class(object_list[[i]]), "pixel_rule")){
      number_of_rules <- number_of_rules + 1
    } else{
      if (identical(class(object_list[[i]]), "pixel_subcat")){
        number_of_subcats <- number_of_subcats + 1
      } else {
        stop("Object ", object_names[i],
             " is neither of class pixel_rule nor of class pixel_subcat.\n",
             call. = FALSE)
      }
    }
  }
  if ((number_of_rules > 0) & (number_of_subcats > 0)){
    stop("The objects must be rules or subcategories, not a mixture",
         .call = FALSE)
  }

  # Creating the object of class "pixel_cat" -----------------------------------
  if (number_of_rules > 0){
    # Creating a subcategory for the set of rules
    new_subcat <- define_subcat("S0", ...=...)
    result <- c(list(name = cat_name,
                     colour = grDevices::col2rgb(cat_colour)/255,
                     subcats = vector(mode = "list", length = 1)))
    result$subcats[[1]] <- new_subcat
    names(result$subcats)[1] <- "S0"
  } else {
    result <- c(list(name=cat_name, colour=grDevices::col2rgb(cat_colour)/255))
    for (i in seq_along(object_list)){
      result$subcats[[i]] <- object_list[[i]]
    }
    names(result$subcats) <- object_names
  }
  class(result) <- "pixel_cat"

  return(result)
}
