#' Creates a subcategory object
#'
#' Creates a list of class \code{pixel_subcat} object from a list of objects of
#' class \code{rule}.
#'
#' @param subcat_name a character string containing the name of the subcategory.
#' @param \dots a list of objects of class \code{rule}.
#'
#' @return An object of class \code{pixel_subcat}, which is a list with the
#'   following elements: \itemize{ \item \code{name} a character string
#'   containing the name of the subcategory. \item \code{rules_list} a list
#'   containing the rules. }
#'
#' @seealso \code{\link{define_rule}}, \code{\link{define_cat}}
#'
#' @examples
#' rule01 <- define_rule("R01", "g", "b", c(0.35, 0.30), c(0.45, 0.10), ">=")
#' rule02 <- define_rule("R02", "g", "b", c(0.35, 0.253), c(0.45, 0.253), ">=")
#'
#' subcl01 <- define_subcat("subcl01", rule01, rule02)
#'
#' @export

define_subcat <- function(subcat_name,  ...){

  rules_list <- list(...)
  rules_names <- vapply(match.call(expand.dots = FALSE)$...,
                        deparse, FUN.VALUE = 'vector')

  for (i in seq_along( rules_list)){
    if (!identical(class( rules_list[[i]]), "rule")){
      stop("Object ", rules_names[i], " is not of class 'rule'.\n",
           "See function define_rule()", call. = FALSE)
    }
  }

  rule_names <- character(length = length( rules_list))
  result <- list("name" = subcat_name,
                 rules = vector(mode = "list", length = length( rules_list)))

  for (i in seq_along( rules_list)){
    result$rules[i] <-  rules_list[i]
  }
  names(result$rules) <- rules_names
  class(result) <- "pixel_subcat"

  return(result)
}
