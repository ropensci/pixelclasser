#' Classifies the pixels of an image
#'
#' Classifies the pixels represented in an object of class
#' \code{transformed_image} using the rules contained in a list of objects of
#' class \code{pixel_cat}.
#'
#' @param image_prop an array containing the transformed colours. It must be an
#'   object produced with function \code{transform_colours()}.
#' @param \dots a list of objects of class \code{pixel_cat} containing the
#'   classification rules.
#' @param unclassed_colour a character string setting the colour to be assigned
#'   to unclassified pixels. Defaults to "black".
#' @param verbose a logical value. When TRUE (default) the function prints some
#'   statistics about the classification.
#'
#' @details This function generates a set of incid matrices indicating
#' whether a pixel belongs to a pixel category or not. An additional matrix is
#' added identifying the pixels that do not belong to the defined categories.
#'
#' @return An object of class \code{classified_image}, which is a list of lists.
#'   Each first-level element corresponds to one of the pixel categories and its
#'   name is the category name. They contains the second-level list, which have
#'   the following elements:
#' \itemize{
#'   \item \code{colour}: a string defining a colour to paint the pixels in the
#'   classified image. Inherited from the
#'   \code{pixel_class} object defining the class.
#'   \item \code{incid_mat}: a logical matrix where \code{TRUE} values indicate
#'   that the pixel belongs to this pixel category.
#' }
#'
#' @seealso \code{\link{define_cat}}, \code{\link[grDevices]{col2rgb}},
#' \code{\link[grDevices]{colours}}
#'
#' @examples
#' \dontrun{
#'
#' # The series of steps to classify a fictitious image
#'
#' yellow <- "#ffcd0eff"
#' blue <- "#5536ffff"
#'
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                           package = "pixelclasser"))
#'
#' rule_01 <- define_rule("rule_01", "g", "b", c(0.345, 1/3), c(0.40, 0.10),
#'                         comparator = "<")
#' rule_02 <- define_rule("rule_02", "g", "b", c(0.345, 1/3), c(0.40, 0.10),
#'                         comparator = ">=")
#'
#' cat_dead_leaves <- define_cat("dead_leaves", blue, rule_01)
#' cat_living_leaves <- define_cat("living_leaves", yellow, rule_02)
#'
#' dead_live_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves,
#'                         cat_living_leaves)
#'
#' }
#'
#' @export

classify_pixels <- function(image_prop, ..., unclassed_colour = "black",
                            verbose = TRUE){

  # Function eval() below needs this definitions to work
  r <- 1
  g <- 2
  b <- 3

  # Checking the categories of the objects -------------------------------------
  cats_list <- list(...)
  num_cats <- length(cats_list)
  cats_names <- sapply(match.call(expand.dots = FALSE)$..., deparse)

  for (i in 1:num_cats){
    if (!identical(class(cats_list[[i]]), "pixel_cat")){
      stop("Object ", cats_names[i], " is not of class pixel_cat.\n",
           "See function define_cat()", call. = FALSE)
    }
  }

  result <- vector(mode = "list", length = num_cats + 1)
  cats_names <- c(cats_names, "unclassified")
  names(result) <- cats_names

  # Creating the incidence matrix corresponding to the unclassified pixels
  result[[num_cats + 1]] <- list("colour" =
                                   (grDevices::col2rgb(unclassed_colour) / 255),
                                "incid_mat" = matrix(nrow = dim(image_prop)[1],
                                                     ncol = dim(image_prop)[2],
                                                     data = TRUE))

  # Auxiliary matrices for temporal data storage
  cat_incid    <- matrix(data = FALSE, nrow = dim(image_prop)[1],
                             ncol = dim(image_prop)[2])
  subcat_incid <- matrix(data = TRUE, nrow = dim(image_prop)[1],
                             ncol = dim(image_prop)[2])

  for (categ in 1:num_cats){
    for (subcateg in seq_along(cats_list[[categ]]$subcats)){
      for (rule in seq_along(cats_list[[categ]]$subcats[[subcateg]]$rules)){
        actual_rule <- cats_list[[categ]]$subcats[[subcateg]]$rules[[rule]]
        subcat_incid <- subcat_incid & eval(parse(text = actual_rule$rule_text))
      }
      cat_incid <- cat_incid | subcat_incid
      subcat_incid[,] <- TRUE
    }

    result[[categ]] <- list("colour" = cats_list[[categ]]$colour,
                                                   "incid_mat" = cat_incid)

    # Setting classified pixels = FALSE in the "unclassified" incidence matrix
    result[[num_cats + 1]]$incid_mat <-
      result[[num_cats + 1]]$incid_mat & (!cat_incid)

    names(result)[categ] <- cats_list[[categ]]$name
    cat_incid[,] <- FALSE
  }

  # Checking for pixels counted twice or more
  classified_pixels <- 0
  for (categ in 1:(num_cats + 1)){
    classified_pixels  <- classified_pixels + sum(result[[categ]]$incid_mat)
    if (verbose){
      message('Pixels in categ ', names(result)[categ],': ',
          sum(result[[categ]]$incid_mat), '\n', sep = '')
    }
  }

  # Checks for errors in the rules
  number_pixels <- length(cat_incid)
  redundant_pixels <- classified_pixels - number_pixels
  if (redundant_pixels != 0){
    warning("There are pixels counted twice. Revise the rules",
            call. = FALSE)
  }

  if (verbose){
    message('Redundant pixels:', redundant_pixels, '\n')
    message('Total number of pixels:', number_pixels, '\n')
  }

  # Creating the output object
  result[[num_cats + 1]]$incid_mat <- !(result[[num_cats + 1]]$incid_mat)
  class(result) <- "classified_image"

  return(result)
}
