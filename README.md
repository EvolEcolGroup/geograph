[![Travis-CI Build Status](https://travis-ci.org/thibautjombart/geograph.svg?branch=master)](https://travis-ci.org/thibautjombart/geograph)



*geograph*: geographic graphs for large-scale spatial modelling
=================================================


Installing *geograph*
-------------

All the following instructions should be entered from a new R session to avoid errors due to installing attached packages.

You may need to install manually the packages *graph* and *RBGL* from *Bioconductor* (try "http" if "https" is not available):
```r
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("RBGL")
```

*devtools* is also needed to install *geograph*:
```r
install.packages("devtools")
```


Then, to install *geograph*, simply type:
```r
library(devtools)
install_github("thibautjombart/geograph")
```

