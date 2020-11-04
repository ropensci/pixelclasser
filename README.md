pixelclasser
============

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

This package contains a set of tools to classify the pixels of digital
images into colour categories arbitrarily defined by the user. It is a
simple version of the multivariate technique known as Support Vector
Machine, adapted to this particular use.

The procedure is simple. A digital image in JPEG or TIFF format is
imported into R. The original image contains three colour variables (or
bands): *R*, *G*, and *B*. The first step is to transform them into
proportions (*r*, *g* and *b*), which simplifies the problem into a
bivariate one. The pixels of the test images can then be represented in
the plane defined by two of the variables (the user judges which two are
more convenient by trial and error) and, hopefully, they would form
separate clusters (pixel categories). The user then traces straight
lines (classification rules) that enclose the pixel clusters. Using the
mathematical expression for these rules and the values of the
transformed variables, each pixel can be tested for pertenence to each
category. This produces a set of logical matrices (incidence matrices)
indicating which pixels belong to each category, stored in appropriate R
objects. These can be submitted to posterior analysis or used to create
a new version of the original image showing the category of each pixel.

`pixelclasser` contains functions to visualize the pixels of the images
and the rules created by the user, to create the rules and to store them
in objects that can be passed to function `classify_pixels()` for the
analysis of the image, and functions to import and export the original
and the classified images.

Installation
------------

You can install the development version from GitHub using `remotes` or
`devtools`

``` r
remotes::install_github("CarlosRealR/pixelclasser", build_vignettes = TRUE)
devtools::install_github("CarlosRealR/pixelclasser", build_vignettes = TRUE)
```

Using pixelclasser
------------------

The workflow and how to use the functions is explained in the vignette
included in the package:

``` r
vignette("pixelclasser")
```

The vignette explains how to use the code and illustrates the procedure
outlined above using a set of images included as package data. This is a
summary of the vignette contents. The following image is the example
used in the vignette: dead, ivy and oak leaves on the soil in the campus
gardens. The small images are representative of each category.

<img src="./inst/extdata/ExampleImages.png" width="50%" style="display: block; margin: auto;" />

This image shows the pixels of the original image (black) and, overlaid,
those of the small images: dead (brown), ivy (blue) and oak leaves
(green). They were plotted using the values of the *g* and *r* colour
variables (the example was an RGB digital image).

<img src="./inst/extdata/ReadMeFig01.png" width="50%" style="display: block; margin: auto;" />

The user defines straight lines that serve as borders of the area
occupied by each pixel category. The figure shows line *L*<sub>1</sub>
which separate the dead leaf pixels from the others, and lines
*L*<sub>2</sub> and *L*<sub>3</sub> which separate the ivy from the oak
pixels. Each line defines two rules (pixels above or underneath the
line). They were defined using the oak pixels but note that ivy pixels
trespass into the oak area, so they were not totally separable.

<img src="./inst/extdata/ReadMeFig02.png" width="50%" style="display: block; margin: auto;" />

Using the mathematical expressions of the rules, the pixels were
classified as belonging or not to each category and classified images
produced. Below are the original image, the image classified using
$L\_{1} alone (dead/fresh leaves) and the full classified image.

<img src="./inst/extdata/ClassifResults.png" width="100%" style="display: block; margin: auto;" />
