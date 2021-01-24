# Creation of rule objects -----------------------------------------------------
rule01 <- list("rule_name"= "Rule01",
               "rule_text" =  "image_prop[,,  r ] > 0.3",
               "comp_op" = ">", "a" = Inf, "c" = 0.3,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.3, 0), "second_point" = c(0.3, 0.55))
class(rule01) <- 'pixel_rule'

rule02 <- list("rule_name"= "Rule02",
               "rule_text" =  "image_prop[,,  r ] <= 0.3",
               "comp_op" = "<=", "a" = Inf, "c" = 0.3,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.3, 0), "second_point" = c(0.3, 0.55))
class(rule02) <- 'pixel_rule'

rule03 <- list("rule_name"= "Rule03",
               "rule_text" = "image_prop[,,  g ] > 0 * image_prop[,, r ] + 0.3",
               "comp_op" = ">", "a" = 0, "c" = 0.3,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.0, 0.3), "second_point" = c(0.55, 0.3))
class(rule03) <- 'pixel_rule'

rule04 <- list("rule_name"= "Rule04",
             "rule_text" = "image_prop[,,  g ] <= 0 * image_prop[,, r ] + 0.3",
               "comp_op" = "<=", "a" = 0, "c" = 0.3,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.0, 0.3), "second_point" = c(0.55, 0.30))
class(rule04) <- 'pixel_rule'

rule05 <- list("rule_name"= "Rule05",
             "rule_text" = "image_prop[,,  g ] > 1 * image_prop[,, r ] + -0.15",
               "comp_op" = ">", "a" = 1, "c" = -0.15,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.15, 0.00), "second_point" = c( 0.55, 0.40))
class(rule05) <- 'pixel_rule'

rule06 <- list("rule_name"= "Rule06",
            "rule_text" = "image_prop[,,  g ] <= 1 * image_prop[,, r ] + -0.15",
               "comp_op" = ">", "a" = 1, "c" = -0.15,
               "x_axis" = "r", "y_axis" = "g",
               "first_point" = c(0.15, 0.00), "second_point" = c( 0.55, 0.40))
class(rule06) <- 'pixel_rule'

# Creation of subclass objects -------------------------------------------------

subcat01 <- list("name" = "Subcat01", "rules" = list("Rule01" = rule01, 
                                                     "Rule04" = rule04))
class(subcat01) <- "pixel_subcat"

subcat02 <- list("name" = "Subcat02", "rules" = list("Rule01" = rule01, 
                                                     "Rule06" = rule06))
class(subcat02) <- "pixel_subcat"

# Creation of class objects ----------------------------------------------------
subcatS0 <- list("name" = "S0", "rules" = list("Rule05" = rule05, 
                                               "Rule04" = rule04))
class(subcatS0) <- "pixel_subcat"

cat_A <- list("name" = "Cat_A", "colour" = grDevices::col2rgb("red")/255,
              "subcats" = list("S0" = subcatS0))
class(cat_A) <- "pixel_cat"

subcatS0 <- list("name" = "S0", "rules" = list("Rule01" = rule01,
                                               "Rule06" = rule06))
class(subcatS0) <- "pixel_subcat"
cat_B <- list("name" = "Cat_B", "colour" = grDevices::col2rgb("green")/255,
              "subcats" = list("S0" = subcatS0))
class(cat_B) <- "pixel_cat"

subcatS0 <- list("name" = "S0", "rules" = list("Rule03" = rule03,
                                               "Rule02" = rule04))
class(subcatS0) <- "pixel_subcat"
cat_C <- list("name" = "Cat_C", "colour" = grDevices::col2rgb("blue")/255,
              "subcats" = list("S0" = subcatS0))
class(cat_C) <- "pixel_cat"

cat_D <- list("name" = "Cat_D", "colour" = (grDevices::col2rgb("green")/255),
              "subcats" = list("subcat01" = subcat01, "subcat02" = subcat02))
class(cat_D) <- "pixel_cat"