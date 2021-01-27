## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(pixelclasser)

## ----echo=FALSE, fig.align='center', out.width = "50%"------------------------
knitr::include_graphics('../inst/extdata/ExampleImages.png')

## -----------------------------------------------------------------------------
ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG", package = "pixelclasser"))
test_ivy_rgb <- read_image(system.file("extdata", "TestIvy.JPG", package = "pixelclasser"))
test_oak_rgb <- read_image(system.file("extdata", "TestOak.JPG", package = "pixelclasser"))
test_dead_rgb <- read_image(system.file("extdata", "TestDeadLeaves.JPG", package = "pixelclasser"))

## -----------------------------------------------------------------------------
transparent_black <- "#00000008"
brown <- "#c86432ff"
yellow <- "#ffcd0eff"
blue <- "#5536ffff"
green <- "#559800ff"

## ---- out.width = "50%", fig.align="center", out.width = "50%"----------------
plot_rgb_plane("r", "b", main = "Image: ivy and oak")
plot_pixels(ivy_oak_rgb, "r", "b", col = transparent_black)

## ---- fig.align="center", out.width = "50%"-----------------------------------
plot_rgb_plane("r", "g", plot_limits = F, plot_guides = F, plot_grid = F)
plot_pixels(ivy_oak_rgb, "r", "g", col = transparent_black)

## ---- fig.align="center", out.width = "50%"-----------------------------------
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33))
plot_pixels(ivy_oak_rgb, "g", "b", col = transparent_black)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)
plot_pixels(test_dead_rgb, "g", "b", col = brown)

## -----------------------------------------------------------------------------
rule_01 <- define_rule("rule_01", "g", "b", list(c(0.345, 1/3), c(0.40, 0.10)), "<")
rule_02 <- define_rule("rule_02", "g", "b", list(c(0.345, 1/3), c(0.40, 0.10)), ">=")

## ---- fig.align="center", out.width = "50%"-----------------------------------
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33))
plot_pixels(ivy_oak_rgb, "g", "b", col = transparent_black)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)
plot_pixels(test_dead_rgb, "g", "b", col = brown)
plot_rule(rule_01, lty = 2, col = brown)

## -----------------------------------------------------------------------------
rule_03 <- define_rule("rule_03","g", "b", list(c(0.35, 0.30), c(0.565, 0.10)), "<")
rule_04 <- define_rule("rule_04","g", "b", list(c(0.35, 0.25), c(0.5, 0.25)), "<")

## ---- fig.align="center", out.width = "50%"-----------------------------------
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33), plot_limits = F, plot_guides = F)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_rule(rule_01, lty = 2, col = green)
plot_rule(rule_03, lty = 2, col = green)
plot_rule(rule_04, lty = 2, col = green)

## ---- fig.align="center", out.width = "50%"-----------------------------------
plot_rgb_plane("g", "b", xlim = c(0.2,0.6), ylim=c(0.1,0.33), plot_limits = F, plot_guides = F)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)

plot_rule(rule_02, lty = 1, col = green)
label_rule(rule_02, label = expression('L'[1]*' (R'[1]*',R'[2]*')'), shift = c(0.035, -0.004), col = green)

plot_rule(rule_03, lty = 1, col = green)
label_rule(rule_03, label = expression('L'[2]*' (R'[3]*',R'[5]*')'), shift = c(0.20, -0.15), col = green)

plot_rule(rule_04, lty = 1, col = green)
label_rule(rule_04, label = expression('L'[3]*' (R'[4]*',R'[6]*')'), shift = c(0.19, 0.0), col = green)

## -----------------------------------------------------------------------------
rule_05 <- define_rule("rule_05", "g", "b", list(c(0.35, 0.30), c(0.565, 0.16)), ">=")
rule_06 <- define_rule("rule_06", "g", "b", list(c(0.35, 0.25), c(0.5, 0.25)), ">=")

## -----------------------------------------------------------------------------
cat_dead_leaves <- define_cat("dead_leaves", blue, rule_01)

## -----------------------------------------------------------------------------
cat_living_leaves <- define_cat("living_leaves", yellow, rule_02)

## -----------------------------------------------------------------------------
cat_oak_leaves <- define_cat("oak_leaves", green, rule_02, rule_03, rule_04)

## -----------------------------------------------------------------------------
subcat_ivy01 <- define_subcat("ivy01", rule_02, rule_06)
subcat_ivy02 <- define_subcat("ivy02", rule_04, rule_05)

cat_ivy_leaves <- define_cat("ivy_leaves", yellow, subcat_ivy01, subcat_ivy02)

## -----------------------------------------------------------------------------
dead_live_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_living_leaves)

## ---- eval=FALSE--------------------------------------------------------------
#  save_classif_image(dead_live_classified, "DeadLiveClassified.JPG", quality = 1)

## -----------------------------------------------------------------------------
ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)

## -----------------------------------------------------------------------------
rule_05 <- define_rule("rule_05", "g", "b", list(c(0.35, 0.30), c(0.565, 0.10)), ">=")
subcat_ivy02 <- define_subcat("ivy02", rule_04, rule_05)
cat_ivy_leaves <- define_cat("ivy_leaves", yellow, subcat_ivy01, subcat_ivy02)
ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)

## ---- eval = FALSE------------------------------------------------------------
#  save_classif_image(ivy_oak_classified, "IvyOakClassified.TIFF")

## ----echo=FALSE, fig.align='center'-------------------------------------------
knitr::include_graphics('../inst/extdata/ClassifResults.png')

