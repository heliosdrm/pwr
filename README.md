# R package &ldquo;pwr&rdquo;
## Basic functions for power analysis

[![Build Status](https://travis-ci.org/heliosdrm/pwr.svg?branch=master)](https://travis-ci.org/heliosdrm/pwr)

[![CRAN version](http://www.r-pkg.org/badges/version/pwr)](http://www.r-pkg.org/pkg/pwr)
[![Downloads](http://cranlogs.r-pkg.org/badges/pwr)]()

This package was originally created by Stephane Champely, from the University of Lyon.

### Getting started

The official release of the package is on CRAN:
http://cran.r-project.org/web/packages/pwr/

To get it installed, just write in an R session:

```R
install.packages("pwr", repos="http://cran.r-project.org")
```

To work with the newest development version, check the build status icon at the top of this page. If it says &ldquo;passing&rdquo;, you should be able to install it from the source. Clone this Git repository in your machine, and if you have the tools to build R packages, do it and install it as appropriate for your OS.

If you cannot build it, you may still install it from an R session (at the expense of not having PDF docs). Set the working directory to the parent folder where `pwr` is copied, and then do:

```R
install.packages("pwr", repos=NULL, type="source")
```

After installing, you have to load to use the package, with:

```R
library(pwr)
```

Feel free to comment on any issues, file bugs or suggest improvements. All contributions are welcome!
