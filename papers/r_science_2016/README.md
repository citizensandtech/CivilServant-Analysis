# Estimating The Effect of Public Postings of Norms in Subreddits: Data and Code
This folder includes data and code associated with the experiment pre-registered on the Open Science Framework on August 25, 2016: [Estimating the Effect of Public Postings of Norms in Subreddits: Pre-Analysis Plan](https://osf.io/jhkcf/). The files in this folder include:

* `science_analysis_final_paper-08.2018.R`: the full R code from the paper
* `r_science_experiment_1_posts.09.26.2016.csv` all posts in the experiment, including randomization blocks removed due to software errors
* `r_science_comments_science_sticky_09.24.2016.csv`: all comments in the experiment, including randomization blocks removed due to software errors
* `r_science_posts_2016.07.04_04.45.23-2016.08.01_22.14.00.csv`: pre-experiment observations used in summary statistics for setting the context

# R Library versions used in this analysis
The following is output from the sessionInfo() command in R from the R version and libraries that were used to generate results:

```
R version 3.5.1 (2018-07-02)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS  10.14.3

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] xtable_1.8-3      beepr_1.3         ri2_0.1.2         estimatr_0.14     randomizr_0.16.1  rms_5.1-2
 [7] SparseM_1.77      Hmisc_4.1-1       Formula_1.2-3     survival_2.43-3   lattice_0.20-38   MASS_7.3-51.1
[13] boot_1.3-20       data.table_1.11.8 pscl_1.5.2        gmodels_2.18.1    lmerTest_3.0-1    lme4_1.1-19
[19] Matrix_1.2-15     texreg_1.36.23    stargazer_5.2.2   lubridate_1.7.4   ggplot2_3.1.0

loaded via a namespace (and not attached):
 [1] splines_3.5.1       gtools_3.8.1        assertthat_0.2.0    latticeExtra_0.6-28 yaml_2.2.0
 [6] numDeriv_2016.8-1   pillar_1.3.1        backports_1.1.3     quantreg_5.38       glue_1.3.0
[11] digest_0.6.18       RColorBrewer_1.1-2  checkmate_1.9.1     minqa_1.2.4         colorspace_1.3-2
[16] sandwich_2.5-0      htmltools_0.3.6     plyr_1.8.4          pkgconfig_2.0.2     purrr_0.2.5
[21] mvtnorm_1.0-8       scales_1.0.0        gdata_2.18.0        MatrixModels_0.4-1  htmlTable_1.13.1
[26] tibble_2.0.0        generics_0.0.2      TH.data_1.0-10      withr_2.1.2         nnet_7.3-12
[31] lazyeval_0.2.1      magrittr_1.5        crayon_1.3.4        polspline_1.1.13    nlme_3.1-137
[36] foreign_0.8-71      tools_3.5.1         multcomp_1.4-8      stringr_1.3.1       munsell_0.5.0
[41] cluster_2.0.7-1     bindrcpp_0.2.2      compiler_3.5.1      rlang_0.3.0.1       grid_3.5.1
[46] nloptr_1.2.1        rstudioapi_0.8      htmlwidgets_1.3     base64enc_0.1-3     gtable_0.2.0
[51] codetools_0.2-16    R6_2.3.0            gridExtra_2.3       zoo_1.8-4           knitr_1.21
[56] dplyr_0.7.8         bindr_0.1.1         stringi_1.2.4       Rcpp_1.0.0          rpart_4.1-13
[61] acepack_1.4.1       audio_0.1-5.1       tidyselect_0.2.5    xfun_0.4
```

