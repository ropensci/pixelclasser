#' Classifies the pixels of an image
#'
#' Classifies the pixels represented in an object of class
#' \code{transformed_image} using the rules contained in a list of objects of
#' class \code{pixel_cat}.
#'
#' @param image_prop an array containing the image. It must be an
#'   object produced with function \code{read_image()}.
#' @param \dots a list of objects of class \code{pixel_cat} containing the
#'   classification rules.
#' @param unclassed_colour a character string setting the colour to be assigned
#'   to unclassified pixels. Defaults to "black".
#' @param verbose a logical value. When TRUE (default) the function prints some
#'   statistics about the classification.
#'
#' @return Returns an object of class \code{classified_image}, which is a list
#'   containing nested lists. Each first-level element corresponds to one of the
#'   pixel categories and its name is the category name. They contains the
#'   second-level list, which have the following elements:
#' \itemize{
#'   \item \code{colour}: a matrix defining a colour to paint the pixels in the
#'   classified image. Inherited from the \code{pixel_class} object defining the
#'   class.
#'   \item \code{incid_mat}: a logical matrix where \code{TRUE} values indicate
#'   that the pixel belongs to this pixel category.
#' }
#'
#' @details This function generates a set of incidence matrices indicating
#'   whether a pixel belongs to a pixel category or not. An additional matrix
#'   identifies the pixels that do not belong to the defined categories, i e
#'   unclassed pixels. Depending on how the rules were defined, it can be void
#'   or contain pixels, but it is always present and named \code{unclassified}.
#'
#'   To create the incidence matrices for each category, a matrix for each rule
#'   is created and then combined with the matrices of the other using the
#'   \code{and} operator.
#'
#'   When a set of subcategories is used, the procedure is the same for each
#'   subcategory and then the matrices of the subcategories are combined again,
#'   this time using the \code{or} operator. See the help for
#'   \code{define_subcat} for more details.
#'
#'   \code{unclassed_colour} can be specified in any form understood by
#'   \code{grDevices::col2grb}.
#'   
#' @seealso \code{\link{define_cat}}, \code{\link[grDevices]{col2rgb}}.
#'
#' @examples
#'
#' # The series of steps to classify a image supplied in the package
#'
#' yellow <- "#ffcd0eff"
#' blue <- "#5536ffff"
#'
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                           package = "pixelclasser"))
#'
#' rule_01 <- define_rule("rule_01", "g", "b",
#'                        list(c(0.345, 1/3), c(0.40, 0.10)), comp_op = "<")
#' rule_02 <- define_rule("rule_02", "g", "b",
#'                        list(c(0.345, 1/3), c(0.40, 0.10)), comp_op = ">=")
#'
#' cat_dead_leaves <- define_cat("dead_leaves", blue, rule_01)
#' cat_living_leaves <- define_cat("living_leaves", yellow, rule_02)
#'
#' ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves,
#'                         cat_living_leaves)
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
  cats_names <- vapply(match.call(expand.dots = FALSE)$...,
                       deparse, FUN.VALUE = 'vector')

  for (i in 1:num_cats){
    if (!identical(class(cats_list[[i]]), "pixel_cat")){
      stop("Object ", cats_names[i], " is not of class pixel_cat.\n",
           "See function define_cat()", call. = FALSE)
    }
  }

  result <- vector(mode = "list", length = num_cats + 2)
  cats_names <- c(cats_names, "unclassified", "summary")
  names(result) <- cats_names

  # Creating the incidence matrix corresponding to the unclassified pixels
  result[[num_cats + 1]] <- list("colour" =
                                 (grDevices::col2rgb(unclassed_colour) / 255),
                                 "incid_mat" = matrix(nrow = dim(image_prop)[1],
                                 ncol = dim(image_prop)[2], data = TRUE))

  # Auxiliary matrices for temporal storage
  cat_incid    <- matrix(data = FALSE, nrow = dim(image_prop)[1],
                             ncol = dim(image_prop)[2])
  subcat_incid <- matrix(data = TRUE, nrow = dim(image_prop)[1],
                             ncol = dim(image_prop)[2])

  for (category in 1:num_cats){
    for (subcateg in seq_along(cats_list[[category]]$subcats)){
      for (rule in seq_along(cats_list[[category]]$subcats[[subcateg]]$rules)){
        actual_rule <- cats_list[[category]]$subcats[[subcateg]]$rules[[rule]]
        subcat_incid <- subcat_incid & eval(parse(text = actual_rule$rule_text))
      }
      cat_incid <- cat_incid | subcat_incid
      subcat_incid[,] <- TRUE
    }

    result[[category]] <- list("colour" = cats_list[[category]]$colour,
                                                   "incid_mat" = cat_incid)

    # Setting classified pixels = FALSE in the "unclassified" incidence matrix
    result[[num_cats + 1]]$incid_mat <-
      result[[num_cats + 1]]$incid_mat & (!cat_incid)

    names(result)[category] <- cats_list[[category]]$name
    cat_incid[,] <- FALSE
  }

  # Checking for pixels counted twice or more
  classified_pixels <- 0
  message_list <- vector(mode = "list", length = (num_cats + 4))
  for (category in 1:(num_cats + 1)){
    classified_pixels  <- classified_pixels + sum(result[[category]]$incid_mat)
    message_list[[category]] <- paste('Pixels in category ',
                                names(result)[category],': ',
                                sum(result[[category]]$incid_mat),
                                '\n', sep = '')
    if (verbose){
      message(message_list[[category]])
    }
  }

  # Checks for errors in the rules
  number_pixels <- length(cat_incid)
  duplicate_pixels <- classified_pixels - number_pixels
  if (duplicate_pixels != 0){
    message_list[[num_cats + 2]] <-
      "There are pixels counted twice. Revise the rules"
    warning(message_list[[num_cats + 2]], call. = FALSE)
  } else {
    message_list[[num_cats + 2]] <- 
      "No pixels were counted twice. The rules seem correct"
  }
  message_list[[num_cats + 3]] <- paste('Duplicate pixels:',
                                        duplicate_pixels, '\n')
  message_list[[num_cats + 4]] <- paste('Total number of pixels:', 
                                        number_pixels, '\n')
  if (verbose){
    message(message_list[[num_cats + 3]])
    message(message_list[[num_cats + 4]])
  }

  # Adding the incidence matrix for unclassified pixels 
  result[[num_cats + 1]]$incid_mat <- !(result[[num_cats + 1]]$incid_mat)
  result$summary <- message_list
  class(result) <- "classified_image"
  return(result)
}
