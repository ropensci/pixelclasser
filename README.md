# pixelclasser

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
bands): \(R\), \(G\), and \(B\). The first step is to transform them
into proportions (\(r\), \(g\) and \(b\)), which simplifies the problem
into a bivariate one. The pixels of the test images can then be
represented in the plane defined by two of the variables (the user
judges which two are more convenient by trial and error) and, hopefully,
they would form separate clusters (pixel categories). The user then
traces straight lines (classification rules) that enclose the pixel
clusters. Using the mathematical expression for these rules and the
values of the transformed variables, each pixel can be classified in one
category. This produces a set of logical matrices (incidence matrices)
indicating which pixels belong to each category, stored in appropriate R
objects. These can be submitted to posterior analysis or used to create
a new version of the original image showing the category of each pixel.

`pixelclasser` contains functions to visualize the pixels of the images
and the rules created by the user, to create the rules and to store them
in objects that can be passed to function `classify_pixels()` for the
analysis of the image, and functions to import and export the original
and the classified images.

## Installation

You can install the last version from the rOpenSci repository in GitHub
using packages `remotes` or `devtools`, which install `remotes`

``` r
remotes::install_github("ropensci/pixelclasser", build_vignettes = TRUE)
devtools::install_github("ropensci/pixelclasser", build_vignettes = TRUE)
```

## Using pixelclasser

The manual with the description of each function and use examples is the
file `/doc/pixelclasser_1.0.0.pdf` (see the link to source code on the
right), but its contents can be found in the Reference section of this
website.

An example session is described in the vignette included in the package,
which can be accessed after installation in the usual way:

``` r
vignette("pixelclasser")
```

It also can be accessed in the section Get started in the top menu of
this page.

# Code of conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
