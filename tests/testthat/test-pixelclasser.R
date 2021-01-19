context('Testing function interfaces')
library(pixelclasser)

# Function read_image() --------------------------------------------------------

test_that("Parameters in read_image() are verified", {

  expect_error(read_image("ExampleImages.png"),
             "The extension of ExampleImages.png is not jpg, tif or equivalent")
})

test_that("Output of read_image() is correct", {

  load(system.file("extdata", "test_image_rgb.R", package = "pixelclasser"))
  new_test_image <- read_image(system.file("extdata", "TestImage.JPG",
                                           package = "pixelclasser"))

  expect_equal(class(test_image_rgb), "transformed_image")
  expect_equal(new_test_image, test_image_rgb)
})

# Function define_rule() -------------------------------------------------------

test_that("Parameters in define_rule() are verified", {

  expect_error(define_rule('R1', 'r', 'b', c(0.1, 0.1), '>'),
               'rule_points must be a list containing two coordinate vectors')
  
  expect_error(define_rule('R1', 'r', 'b', list(c(0.1, 0.1)), '>'),
               'rule_points must contain two points')
  
  expect_error(define_rule('R1', 'p', 'b', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
               'The x_axis must be one of "r", "g" or "b"')

  expect_error(define_rule('R1', 'r', 'B', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
               'The y_axis must be one of "r", "g" or "b"')

  expect_error(define_rule('R1', 'b', 'b', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
               'x_axis and y_axis must be different')

  expect_error(define_rule('R1', 'r', 'g', list(c(0.5, 0.5), c(0.5, 0.5)), '>'),
               'Start and end points are the same')

  expect_error(define_rule('R1', 'r', 'g', list(c(0.1, 0.1), c(0.5, 0.5)), '$'),
               'The comparation operator must be one of ">", ">=", "<" or "<="')
})

test_that("Output of define_rule() is correct", {

  load(system.file("extdata", "test_rules.R", package = "pixelclasser"))
  new_rule01 <- define_rule('Rule01', 'r', 'g',
                            list(c(0.3, 0.0), c(0.3, 0.55)), comp.op = '>')
  new_rule03 <- define_rule('Rule03', 'r', 'g',
                            list(c(0.0, 0.3), c(0.55, 0.3)), comp.op = '>')
  new_rule05 <- define_rule('Rule05', 'r', 'g',
                            list(c(0.15, 0.0), c(0.55, 0.4)), comp.op = '>')

  expect_equal(class(new_rule01), "pixel_rule")
  expect_equal(new_rule01, rule01)
  expect_equal(new_rule03, rule03)
  expect_equal(new_rule05, rule05)
})

# Function define_subcat() -----------------------------------------------------

test_that("Parameters in define_subcat() are verified", {

  load(system.file("extdata", "test_rules.R", package = "pixelclasser"))
  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))

  expect_error(subcat01 <- define_subcat('Subcat01', rule01, subcat01))
})

test_that("Output of define_subcat() is correct", {

  load(system.file("extdata", "test_rules.R", package = "pixelclasser"))
  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))

  new_subcat01 <- define_subcat('Subcat01', rule01, rule04)

  expect_equal(class(new_subcat01), "pixel_subcat")
  expect_equal(new_subcat01, subcat01)
})

# Function define_cat() --------------------------------------------------------

test_that("Parameters in define_cat() are verified", {

  load(system.file("extdata", "test_rules.R", package = "pixelclasser"))
  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))

  expect_error(new_cat_A <- define_cat('Cat_A', 'red', rule05, subcat01),
               "The objects must be rules or subcategories, not a mixture")
  expect_error(new_cat_A <- define_cat('Cat_A', 'red', 'A', 'B'))
})

test_that("Output of define_cat() is correct", {

  load(system.file("extdata", "test_rules.R", package = "pixelclasser"))
  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))
  load(system.file("extdata", "test_cat.R", package = "pixelclasser"))

  new_cat_A <- define_cat('Cat_A', 'red', rule05, rule04)

  expect_equal(class(new_cat_A), "pixel_cat")
  expect_equal(new_cat_A, cat_A)
})

# Function classify_pixels() ---------------------------------------------------

test_that("Parameters in classify_pixels() are verified", {

  load(system.file("extdata", "test_cat.R", package = "pixelclasser"))

  expect_error(test_image_classif <- classify_pixels(test_image_rgb,
                                                     list('A', 'B'),
                                                     cat_B, cat_C))
})

test_that("Output of classify_pixels() is correct", {

  load(system.file("extdata", "test_image_rgb.R",
                   package = "pixelclasser"))
  load(system.file("extdata", "test_cat.R",
                   package = "pixelclasser"))
  load(system.file("extdata", "test_image_classified.R",
                   package = "pixelclasser"))

  new_test_image_classif <- classify_pixels(test_image_rgb,
                                            cat_A, cat_B, cat_C)

  expect_equal(new_test_image_classif, test_image_classif)
})

# Function plot_rule() ---------------------------------------------------------

test_that("Parameters in plot_rule() are verified", {

  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))
  plot_rgb_plane('r', 'g')

  expect_error(plot_rule(rule = subcat01))
})

# Function label_rule() --------------------------------------------------------

test_that("Parameters in label_rule() are verified", {
  load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))
  
  expect_error(label_rule(rule = subcat01))
})

# Function save_classif_image() ------------------------------------------------
test_that("Parameters in save_classif_image() are verified", {

  load(system.file("extdata", "test_image_classified.R",
                   package = "pixelclasser"))

  expect_error(save_classif_image(test_image_classif,
                               file = "../vignettes/test_image_classified.txt"))
})
