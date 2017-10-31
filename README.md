# POSAC (Partial Order Scalogram Analysis with Base Co-ordinates)

Maintainer: Cambridge Assessment, Assessment, Research and Development (ARD) <benton.t@cambridgeassessment.org.uk>

Authors: Tom Benton & Tom Bramley

## About

An R package to carry out POSAC analysis, a technique used in Facet Theory.

Borg, I., & Shye, S. (1995). Facet Theory: form and content. Thousand Oaks: CA: SAGE.

[Shye,S. (2009) POSAC in 4 simple steps](https://www.researchgate.net/profile/Samuel_Shye/publication/263932933_PARTIAL_ORDER_SCALOGRAM_ANALYSIS_BY_COORDINATES_POSAC_AS_A_FACET_THEORY_MEASUREMENT_PROCEDURE_HOW_TO_DO_POSAC_IN_FOUR_SIMPLE_STEPS/links/0a85e53c638c645503000000.pdf)

Note that POSAC works best as a small data technique. In particular, if you have a very large number of variables 
don't be surprised if analysis fails to produce useful results.

## Installation

This package can be installed using the devtools package using the commands below.

library(devtools)
install_github("CambridgeAssessmentResearch/POSAC")

In order to run all of the examples provided with this package you will first need to install the packages ggplot2, foreach and hasseDiagram from CRAN, and the package Rgrpahviz from Bioconductor. This can be done with the commands below.

source("https://bioconductor.org/biocLite.R")

biocLite("Rgraphviz")

install.packages("ggplot2","hasseDiagram","foreach")

## License

The MIT License (MIT)

Copyright (c) 2017 Cambridge Assessment

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
