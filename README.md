---
output: github_document
---

# Purpose
This is a package for UW BIOST 561 Final Project (Spring 2024).

- The URL to the GitHub (i.e., the source code) is: https://github.com/WinX23/UWBiost561FP
- The URL to the Pkgdown webpage is: https://winx23.github.io/UWBiost561FP/

# How to install
This package is called `UWBiost561FP`. To install, run the following code (in R):

```R
library(devtools)
devtools::install_github("YxWr5/UWBiost561FP")
```

Upon completion, you can run the following code (in R):
```R
library(UWBiost561FP)
```

# Dependencies

The package depends on the following packages: `expm`, `tidyr`, `dplyr`, and `ggplot2`.

# Session info

This package was developed in the following environment
```R
> devtools::session_info()
- Session info ----------------------------------------------------------------
 setting  value
 version  R version 4.3.3 (2024-02-29 ucrt)
 os       Windows 11 x64 (build 22631)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United States.utf8
 ctype    C
 tz       America/Los_Angeles
 date     2024-06-05
 rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
 pandoc   3.1.1 @ C:/R Programming/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

- Packages --------------------------------------------------------------------
 ! package      * version    date (UTC) lib source
   brio           1.1.4      2023-12-10 [1] CRAN (R 4.3.3)
   bslib          0.7.0      2024-03-29 [1] CRAN (R 4.3.3)
   cachem         1.0.8      2023-05-01 [1] CRAN (R 4.3.3)
   callr          3.7.6      2024-03-25 [1] CRAN (R 4.3.3)
   cli            3.6.2      2023-12-11 [1] CRAN (R 4.3.3)
   colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.3.3)
   commonmark     1.9.1      2024-01-30 [1] CRAN (R 4.3.3)
   crayon         1.5.2      2022-09-29 [1] CRAN (R 4.3.3)
   curl           5.2.1      2024-03-01 [1] CRAN (R 4.3.3)
   desc           1.4.3      2023-12-10 [1] CRAN (R 4.3.3)
   devtools       2.4.5      2022-10-11 [1] CRAN (R 4.3.3)
   digest         0.6.35     2024-03-11 [1] CRAN (R 4.3.3)
   dplyr          1.1.4      2023-11-17 [1] CRAN (R 4.3.3)
   ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.3.3)
   evaluate       0.23       2023-11-01 [1] CRAN (R 4.3.3)
   expm           0.999-9    2024-01-11 [1] CRAN (R 4.3.3)
   fansi          1.0.6      2023-12-08 [1] CRAN (R 4.3.3)
   farver         2.1.1      2022-07-06 [1] CRAN (R 4.3.3)
   fastmap        1.1.1      2023-02-24 [1] CRAN (R 4.3.3)
   fs             1.6.3      2023-07-20 [1] CRAN (R 4.3.3)
   generics       0.1.3      2022-07-05 [1] CRAN (R 4.3.3)
   ggplot2        3.5.0      2024-02-23 [1] CRAN (R 4.3.3)
   glue           1.7.0      2024-01-09 [1] CRAN (R 4.3.3)
   gtable         0.3.4      2023-08-21 [1] CRAN (R 4.3.3)
   highr          0.10       2022-12-22 [1] CRAN (R 4.3.3)
   hms            1.1.3      2023-03-21 [1] CRAN (R 4.3.3)
   htmltools      0.5.8      2024-03-25 [1] CRAN (R 4.3.3)
   htmlwidgets    1.6.4      2023-12-06 [1] CRAN (R 4.3.3)
   httpuv         1.6.15     2024-03-26 [1] CRAN (R 4.3.3)
   jquerylib      0.1.4      2021-04-26 [1] CRAN (R 4.3.3)
   jsonlite       1.8.8      2023-12-04 [1] CRAN (R 4.3.3)
   knitr          1.45       2023-10-30 [1] CRAN (R 4.3.3)
   labeling       0.4.3      2023-08-29 [1] CRAN (R 4.3.1)
   later          1.3.2      2023-12-06 [1] CRAN (R 4.3.3)
   lattice        0.22-5     2023-10-24 [1] CRAN (R 4.3.3)
   lifecycle      1.0.4      2023-11-07 [1] CRAN (R 4.3.3)
   magrittr       2.0.3      2022-03-30 [1] CRAN (R 4.3.3)
   Matrix         1.6-5      2024-01-11 [1] CRAN (R 4.3.3)
   memoise        2.0.1      2021-11-26 [1] CRAN (R 4.3.3)
   mime           0.12       2021-09-28 [1] CRAN (R 4.3.1)
   miniUI         0.1.1.1    2018-05-18 [1] CRAN (R 4.3.3)
   munsell        0.5.1      2024-04-01 [1] CRAN (R 4.3.3)
   pillar         1.9.0      2023-03-22 [1] CRAN (R 4.3.3)
   pkgbuild       1.4.4      2024-03-17 [1] CRAN (R 4.3.3)
   pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.3.3)
   pkgdown        2.0.7      2022-12-14 [1] CRAN (R 4.3.3)
   pkgload        1.3.4      2024-01-16 [1] CRAN (R 4.3.3)
   prettyunits    1.2.0      2023-09-24 [1] CRAN (R 4.3.3)
   processx       3.8.4      2024-03-16 [1] CRAN (R 4.3.3)
   profvis        0.3.8      2023-05-02 [1] CRAN (R 4.3.3)
   promises       1.2.1      2023-08-10 [1] CRAN (R 4.3.3)
   ps             1.7.6      2024-01-18 [1] CRAN (R 4.3.3)
   purrr          1.0.2      2023-08-10 [1] CRAN (R 4.3.3)
   R6             2.5.1      2021-08-19 [1] CRAN (R 4.3.3)
   rcmdcheck      1.4.0      2021-09-27 [1] CRAN (R 4.3.3)
   Rcpp           1.0.12     2024-01-09 [1] CRAN (R 4.3.3)
   readr          2.1.5      2024-01-10 [1] CRAN (R 4.3.3)
   remotes        2.5.0      2024-03-17 [1] CRAN (R 4.3.3)
   rlang          1.1.3      2024-01-10 [1] CRAN (R 4.3.3)
   rmarkdown      2.26       2024-03-05 [1] CRAN (R 4.3.3)
   roxygen2       7.3.1      2024-01-22 [1] CRAN (R 4.3.3)
   rprojroot      2.0.4      2023-11-05 [1] CRAN (R 4.3.3)
   rstudioapi     0.16.0     2024-03-24 [1] CRAN (R 4.3.3)
   sass           0.4.9      2024-03-15 [1] CRAN (R 4.3.3)
   scales         1.3.0      2023-11-28 [1] CRAN (R 4.3.3)
   sessioninfo    1.2.2      2021-12-06 [1] CRAN (R 4.3.3)
   shiny          1.8.1.1    2024-04-02 [1] CRAN (R 4.3.3)
   stringi        1.8.3      2023-12-11 [1] CRAN (R 4.3.2)
   stringr        1.5.1      2023-11-14 [1] CRAN (R 4.3.3)
   testthat     * 3.2.1      2023-12-02 [1] CRAN (R 4.3.3)
   tibble         3.2.1      2023-03-20 [1] CRAN (R 4.3.3)
   tidyr          1.3.1      2024-01-24 [1] CRAN (R 4.3.3)
   tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.3.3)
   tzdb           0.4.0      2023-05-12 [1] CRAN (R 4.3.3)
   urlchecker     1.0.1      2021-11-30 [1] CRAN (R 4.3.3)
   usethis        2.2.3      2024-02-19 [1] CRAN (R 4.3.3)
   utf8           1.2.4      2023-10-22 [1] CRAN (R 4.3.3)
 P UWBiost561FP * 0.0.0.9000 2024-06-05 [?] load_all()
   vctrs          0.6.5      2023-12-01 [1] CRAN (R 4.3.3)
   withr          3.0.0      2024-01-16 [1] CRAN (R 4.3.3)
   xfun           0.43       2024-03-25 [1] CRAN (R 4.3.3)
   xml2           1.3.6      2023-12-04 [1] CRAN (R 4.3.3)
   xopen          1.0.0      2018-09-17 [1] CRAN (R 4.3.3)
   xtable         1.8-4      2019-04-21 [1] CRAN (R 4.3.3)
   yaml           2.3.8      2023-12-11 [1] CRAN (R 4.3.2)

 [1] C:/R Programming/R-4.3.3/library
```
