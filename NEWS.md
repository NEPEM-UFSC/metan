# metan 1.19.0
## New features
* New "complementarity" object returned in the function `mgidi()`.


# metan 1.18.0
## New features
* New functions `*_wd_here()` to set and get the Working Directory (wd) quicky.
  - `get_wd_here()` gets the working directory to the path of the current script.
  - `set_wd_here()` sets the working directory to the path of the current script.
  - `open_wd_here()` open the File Explorer at the directory path of the current script.
  - `open_wd()` open the File Explorer at the current working directory.
* `corr_coef()` now can compute both linear and partial correlation, controled by the argument `type`.
* New function `network_plot()` to produce a network plot of a correlation matrix or an object computed with `corr_coef()`. 
* New functions `sample_random()` and `sample_systematic()` for random and systematic sampling, respectively.


## Minor improvements

* `plot.waasb()` now has new arguments to control whether to show the percentage values within bars and the order of variables on the x-axis.
* `corr_coef()` now handles grouped data passed from `group_by()`
* New arguments `size.varnames` and `col.varnames` added in `corr_plot()`.
* Fix bug in `gmd(mod, "h2")`, when `mod` is computed with `random = "env"`.
* Include the argument `repel` in `plot.gge()`.


# metan 1.17.0
## New features
* Implement a `plot` method for `path_coeff_*()` functions.
* New function `path_coeff_seq()` to implement a sequential (two chains) path analysis.
* New function `prop_na()` to measure the proportion of `NAs` in each column.
* New functions `remove_cols_all_na()` and `remove_rows_all_na()` to remove columns and rows that have all values as `NAs`.
* New functions `ci_mean_z()` and `ci_mean_t()` to compute z- and t-confidence intervals, respectively.
* New function `set_wd_here()` to set the working directory to the path of the current script.

## Minor improvements
* Fix bug in `rowname_to_column()`.
* Fix bug in `mps()` where stab was being rewritten with stab_res.
* Changes the object name in `mgidi()` example that overwrites the function.
* Fix bug with `x.lab` and `y.lab` from `plot_scores()`. Now it accepts an object from `expression()`
* `plot_waasby()` now accepts objects of class `waas_means`.
* `get_model_data()` now includes new options `coefs`, and `anova` for objects computed with `ge_reg()`.
* New argument `max_overlaps` in `plot_scores()` to exclude text labels that overlap too many things.
* Improve the control over highlighted individuals in `plot_scores()` (shape, alpha, color, and size).


# metan 1.16.0
## New features
* Include new AMMI-based stability methods.
* Update `ge_stats()` to include the new stability methods.
* `wsmp()` now accepts objects computed with `mps()`, `waas()`, and `waasb()`.

## Minor improvements

* `AMMI_indexes()` has been deprecated in favour of `ammi_indexes()`.
* Include formulas for the AMMI-indexes in `ammi_indexes()`
* Correct the number of environments in the documentation of `data_ge()`.

# metan 1.15.0
* Fix bug when calling `gmd(., "data")`
* Fix bug with `fai_blup()` when genotypes has distance as 0.
* Fix bug in `inspect()` when some trait has character values.
* Fix bug in `gmd(model, "blupge")`

# metan 1.14.0
## Minor improvements
* Fix bug in `get_model_data()` calling objects of class `mgidi` with `what = "PCA"`.
* Fix bug in `path_coeff()` when generating sequences of direct effects depending on the constant added to the diagonal of correlation matrix.
* Improve output of `gmd()` for `gge` objects.
* New option `projection` in `gmd()` for `gge` objects to get the projection of each genotype in the AEC coordinates.
* Fix bug when using `mtsi()` with an object of class `waas`.

# metan 1.13.0
## New functions
* `progress()` and `run_progress()` for text progress bar in the terminal.
* `rbind_fill_id()` To implement the common pattern of `do.call(rbind, dfs)` with data frame identifier and filling of missing values.
* `hmgv()`, `rpgv()`, `hmrpgv()`, `blup_indexes()` to compute stability indexes based on a mixed-effect model.
* `mps()` and `mtmps()` for uni- and multivariate-based mean performance and stability in multi-environment trials.

## Minor improvements
* `ge_reg()` now returns hypotesis testing for slope and deviations from the regression. Thanks to [@LeonardoBehring](https://www.researchgate.net/profile/Leonardo-Bhering) and [@MichelSouza](https://www.escavador.com/sobre/6363700/michel-henriques-de-souza) for the suggestion.
* `Resende_indexes()` now remove `NA`s before computing harmonic and arithmetic means.
* Improved outputs in `plot_scores` that now has a `highlight` argument to highlight genotypes or environments by hand. Thanks to [Ibrahim Elbasyoni](https://scholar.google.com/citations?user=zPJjnSEAAAAJ&hl=en&authuser=1) for his suggestions.
* [Licecycle badges](https://lifecycle.r-lib.org/articles/stages.html) added to the functions' documentation.
* Fix bug in `clustering()` when using  with `by` argument and defacult `nclust` argument.
* `get_model_data()` now extract BLUEs from objects computed with `gamem()` and `gamem_met()`. Thanks to [@MdFarhad](https://www.researchgate.net/profile/Md-Farhad-2) for suggesting me this improvement.
* `g_simula()` and `ge_simula()` now have a `res_eff` to control the residual effect.
* `mgidi()` now have an optional `weights` argument to assign different weights for each trait in the selection process. Thanks to [@MichelSouza](https://www.escavador.com/sobre/6363700/michel-henriques-de-souza) for his suggestion.


# metan 1.12.0
## New functions
* `get_levels_comb()` to get the combination of the levels of a factor.
* `g_simula()` to simulate replicated genotype data.
* `add_row_id()` to add a column as the row id.
* `remove_rownames()`, `column_to_rownames()` and `rownames_to_column()` to deal
with rownames.

## Minor improvements
* New argument `sel.var()` in `corr_ci()` to filter correlations with a selected
variable
* New arguments `fill` and `position.fill` in `plot_ci()` to fill correlations
by levels of a factor variable.
* Remove deprecated arguments in `arrange_ggplot()` and `gge()`.
* New argument `theme` `in arrange_ggplot()` to control the theme of the plot.
* Include `by` argument in `gafem()`.
* `mgid()` now understands models of class `gafem_grouped`.
* Fix bug in `get_levels()` to get the levels even if the variable is not a factor.


# metan 1.11.0
## New functions
* `set_union()`, `set_difference()` and `set_intersect()` for set operations with many sets.
* `venn_plot()` to produce Venn diagrams.

## Minor improvements
* `gge()` now have a `by` argument and understand data passed from `group_by`.
* New arguments `col.stroke` and `size.stroke` in `plot.gge()`
* `gtb` and `gytb` now produces biplots with lines for genotype's vectors in `type = 1`.
* `get_model_data()` now understand objects of class `fai_blup` and `sh`.

# metan 1.10.0
## New functions
* `get_dist()` to get distance matrices from objects of class `clustering`.
* `get_corvars()` to get normal, multivariate correlated variables.
* `get_covmat()` to obtain covariance matrix based on variances and correlation values.
* `as_numeric()`, `as_integer()`, `as_logical()`, `as_character()`, and `as_factor()` to coerce variables to specific formats quickly.
* `n_valid()`, `n_missing()`, and `n_unique()` to count valid, missing, and unique values, respectively.
* `tidy_colnames()` to clean up column names. It is a shortcut to `tidy_strings()` applied to the column names of a data frame.
* `env_stratification()` to perform environment stratification using factor analysis.

## Minor improvements
* `as_*()` now handles vectors.
* `plot.corr_coef()` now shows both stars or p-values for reporting the significance of correlations.

* `gamem()`, `gamem_met()`, and `waasb()` now have a `by` argument and understand data passed from `group_by`.
* `mtsi()` and `mgidi()` now returns the ranks for the contribution of each factor and understand models fitted with `gamem()` and `waasb()` using the `by` argument.
* `plot.mtsi()` and `plot.mgidi()` now returns a radar plot by default when using `type = "contribution"`.
* `get_model_data()` now returns the genotypic and phenotypic correlation matrices from objects of class `waasb` and `gamem`.
* `replace_string()`, `replace_number()`, `extract_string()`, and `extract_number()` now accepts [tidy evaluation](https://tidyselect.r-lib.org/articles/syntax.html) in the new `...` argument.

# metan 1.9.0
* New functions `add_prefix()` and `add_suffix()` to add prefixes and suffixes to variable names, respectively.
* New function `select_pred()` to selects a best subset of predictor variables.
* New function `acv()` to compute the [adjusted coefficient of variation](https://linkinghub.elsevier.com/retrieve/pii/S1161030118301904) to account for the systematic dependence of $\sigma^2$ from $\mu$.
* New function `ge_acv()` to compute yield stability index based on the adjusted coefficient of variation.
* New function `ge_polar()` to compute yield stability index based on [Power Law Residuals (POLAR) statistics](https://linkinghub.elsevier.com/retrieve/pii/S0378429015300368).
* New function `mantel_test()` to performs a Mantel test between two matrices. 
* New arguments `prefix` and `suffix` in `concatenate()` to add prefixes and suffixes to concatenated values, respectively.
* List packages providing the Rd macros in 'Imports' instead of 'Suggests' as suggested by the CRAN team.

# metan 1.8.1
* Use `\doi{}` markup in Rd files.

# metan 1.8.0
* New function `gytb()` to generate the Genotype by yield*trait biplot.
* New functions `row_col_mean()` and `row_col_sum()`to add a row with the mean/sum of each variable and a column with the mean/sum for each row of a matrix or data frame.
* New functions `has_zero()`, `remove_cols_zero()`, `remove_rows_zero()`, `select_cols_zero()`, `select_rows_zero()`, and `replace_zero()` to deal with 0s in a data frame.
* Fix bug of inconsistent color legend when plotting objects of class `gge`.
* Include class `gge` and `can_corr` in `get_model_data()`.
* New argument `position` in `plot.gamem()` and `plot.mtsi()`  to control the position adjustment of the bar plot.
* New argument `col.by` in `corr_plot()` to map the color of the points by a categorical variable.
* New argument `use_data` in functions `mgidi()`, `fai_blup()`, and `Smith_Hazel()` to control which data is used (BLUPs or phenotypic means) to compute the index.
* `inspect()` now generate a warning if zero values are observed in data.


# metan 1.7.0

* New functions `clip_read()` and `clip_write()` to read from the clipboard and write to the clipboard, respectively.
* New function `sum_by()` to compute the sum by levels of factors.
* Update wsmp.R ([#7](https://github.com/TiagoOlivoto/metan/pull/7)). Thank you @[BartoszKozak](https://github.com/bartosz-kozak) for your contribution.
* `mgidi()` now allows using a fixed-effect model fitted with `gafem()` as input data.
* `round_cols()` now can be used to round whole matrices.

# metan 1.6.1
* `plot.mgidi()` can now plot the contribution for all genotypes.
* `plot_bars()` and `plot_factbars()` now shows the values with `values = TRUE`
* Update the functions by using the new `dplyr::across()`
* Update citation field by including number and version of the published paper.

# metan 1.6.0
## New functions
* `Smith_Hazel()` and `print.sh()` and `plot.sh()` for computing the Smith and Hazel selection index.
* `coincidence_index()` for computing the coincidence index.

## Minor improvements
* `get_model_data()` now extracts the genotypic and phenotypic variance-covariance matrix from objects of class `gamem` and `waasb`.
* `fai_blup()` now returns the total genetic gain and the list with the ideotypes' construction.
* `mgidi()` now computes the genetic gain when a mixed-model is used as input data.
* The S3 method [`plot()`](https://tiagoolivoto.github.io/metan/reference/plot.mgidi.html) for objects of class `mgidi` has a new argument `type = "contribution"` to plot the contribution of each factor in the MGIDI index.
* `plot_scores()` now can produce a biplot showing other axes besides PC1 and PC2. To change the default IPCA in each axis use the new arguments `first` and `second`.


# metan 1.5.1
## Minor changes
* `plot_bars()` and `plot_factbars()` now align vertically the labels to the error bars.
* `fai_blup()` now returns the eigenvalues and explained variance for each axis and variables into columns instead row names.
* Fixes the error with `donttest{}` examples. Now, the correct data set is used in the example of `fai_blup()`


# metan 1.5.0
## New functions
* `select_rows_na()` and `select_cols_na()` to select rows or columns with with `NA` values.
* `mgidi()` to compute the multi-trait genotype-ideotype distance index.
* `plot_bars()` to create bar plots quickly. Thanks to [@MariaDiel](https://www.researchgate.net/profile/Maria-Diel) for her suggestion. 


## Minor changes
* Deprecated functions `hm_mean()` and `gm_mean()` removed in favour of `hmean()` and `gmean()`, respectively.
* Deprecated argument `rep` retired in `Fox()`, `ge_effects()`, `Huehn()`, `resp_surf()`, `superiority()`, and `Thennarasu()`
* Deprecated argument `verbose` retired in `anova_ind()`
* Deprecated argument `region` retired in `resp_surf()`
* Remove dependency on dendextend by using ggplot2-based graphics in `plot.wsmp()`.
* Update package site with [pkgdown v1.5.0](https://pkgdown.r-lib.org/news/index.html).
* Update documentation in `ge_plot()`
* Allow using `fai_blup()` with `gamem()`
* Improve checking process with `inspect()`
* Improve feedback for results, indicating random and fixed effects. Thanks to [@NelsonJunior](https://scholar.google.com.br/citations?user=i2F6X04AAAAJ&hl=pt-BR) for his suggestion.
* `plot()` call on objects of class `gamem`, `waasb` and `waas` now returns the variable names automatically. Thanks to [@MdFarhad](https://www.researchgate.net/profile/Md-Farhad) for suggesting me this change.
* `plot.gamem()` and `plot.waasb()` have a new argument (`type = "vcomp"`) to produce a plot showing the contribution of the variance components to the phenotypic variance
* `cv_ammi()`, `cv_ammif()`, and `cv_blup()` now check for missing values and unbalanced data before computing the cross-validation. ([#3](https://github.com/TiagoOlivoto/metan/issues/3))


## Bug fixes
* Fix problems from a recent upgrade of package `tibble` to version 3.0.0.
* `get_model_data()` now fills rows that don't matches across columns with `NA`. Thanks to [@MdFarhad](https://www.researchgate.net/profile/Md-Farhad) for his report.
* `get_model_data()` called now report mean squares, F-calculated and P-values for blocks within replicates in `anova_ind()`.


# metan 1.4.0
## Bug fixes
* Factor columns can now have custom names rather than `ENV`, `GEN`, and `REP` only ([#2](https://github.com/TiagoOlivoto/metan/issues/2)).

## New functions
* `gmd()` a shortcut to `get_model_data()`
* `gtb()` to generate a genotype-by-trait biplot.
* `gamem_met()` to analyze genotypes in multi-environment trials using mixed- or random-effect models allowing unbalanced data. Thanks to [@EderOliveira](https://www.embrapa.br/en/web/portal/team/-/empregado/321725/eder-jorge-de-oliveira) for his e-mail.
* `has_class()` to check if a class exists.
* `impute_missing_val()` to impute missing values in a two-way table based on Expectation-Maximization algoritms. 
* `non_collinear_vars()` to select a set of predictors with minimal multicollinearity.
* `replace_na()` to replace `NA` values quicly.
* `random_na()` to generate random `NA` values based on a desired proportion.


## Minor changes
* `gge()`, `performs_ammi()`, `waas()`, and `waasb()` now handle with unbalanced data by implementing a low-rank matrix approximation using singular value decomposition to impute missing entires. Imputation generates a warning message.
* `NA` values are checked and removed with a warning when computing stability indexes. Thanks to [@MdFarhad](https://www.researchgate.net/profile/Md-Farhad) for alerting me.
* New argument `plot_res` in `path_coeff()` to create a residual plot of the multiple regression model.
* Update the citation file to include the [published official reference](https://doi.org/10.1111/2041-210X.13384).
* Argument `verbose` deprecated in functions `anova_ind()` and `split_factors()`
* Argument `rep` deprecated in functions `Fox()`, `Huehn()`, `superiority()`, and `Thennarasu()`.
* Deprecated argument `means_by` removed in functions `can_corr()` and `clustering()`.
* Deprecated argument `verbose` removed in functions `colindiag()` and `split_factors()`.
* Deprecated argument `values` removed in functions `desc_stat()` and `find_outliers()`.
* Deprecated argument `var` removed in function `desc_wider()`.
* Remove dependency on lattice by using ggplot2 in `plot.resp_surf()`.
* An up-to-date cheat sheet was included.


# metan 1.3.0
## New functions
   - `alpha_color()` To get a semi-transparent color
   - `gafem()` To analyze genotypes using fixed-effect models.
   - `residual_plots()` A helper function to create residuals plots.
   - `stars_pval()` To generate significance stars from p-values
   - `doo()` An alternative to `dplyr::do` for doing anything
   
### utils_stats
   - `cv_by()` For computing coefficient of variation by levels of a factor.
   - `max_by()` For computing maximum values by levels of a factor.
   - `means_by()` For computing arithmetic means by levels of a factor.
   - `min_by()` For computing minimum values by levels of a factor.
   - `n_by()` For getting the length.
   - `sd_by()` For computing sample standard deviation.
   - `sem_by()` For computing standard error of the mean by levels of a factor.
   - `av_dev()` computes the average absolute deviation.
   - `ci_mean()` computes the confidence interval for the mean.
   - `cv()` computes the coefficient of variation.
   - `hm_mean()`, `gm_mean()` computes the harmonic and geometric means, respectively. The harmonic mean is the reciprocal of the arithmetic mean of the reciprocals. The geometric mean is the nth root of n products.
   - `kurt()` computes the kurtosis like used in SAS and SPSS.
   - `range_data()` Computes the range of the values.
   - `sd_amo()`, `sd_pop()` Computes sample and populational standard deviation, respectively.
   - `sem()` computes the standard error of the mean.
   - `skew()` computes the skewness like used in SAS and SPSS.
   - `sum_dev()` computes the sum of the absolute deviations.
   - `sum_sq_dev()` computes the sum of the squared deviations.
   - `var_amo()`, `var_pop()` computes sample and populational variance.
   - `valid_n()` Return the valid (not NA) length of a data.

### utils_rows_cols
   - `colnames_to_lower()`, `colnames_to_upper()`, and `colnames_to_title()` to translate column names to lower, upper and title cases quickly.

### utils_num_str
   - `all_lower_case()`, `all_upper_case()`, and `all_title_case()` to translate strings vectors or character columns of a data frame to lower, upper and title cases, respectively.
   - `tidy_strings()` Tidy up characters strings, non-numeric columns, or any selected columns in a data frame by putting all word in upper case, replacing any space, tabulation, punctuation characters by `'_'`, and putting `'_'` between lower and upper cases.
   - `find_text_in_num()` Find text fragments in columns assumed to be numeric. This is especially useful when `everything()` is used in argument `resp` to select the response variables.


## New arguments
   - `anova_ind()`, `anova_joint()`, `performs_ammi()`, `waas()` and `waasb()`,  now have the argument `block` to analyze data from trials conducted in an alpha-lattice design. Thanks to [@myaseen208](https://twitter.com/myaseen208?lang=en) for his suggestion regarding multi-environment trial analysis with alpha-lattice designs.
   - argument `repel` included in `plot_scores()` to control wheater the labels are repelled or not to avoid overlapping.

## Deprecated arguments
   Argument `means_by` was deprecated in functions `can_corr()` and `clustering()`. Use `means_by()` to pass data based on means of factor to these functions.
   
## Minor changes

   - Change "#000000FF" with "#FFFFFF00" in `transparent_color()`
   - `desc_stat()` now handles grouped data passed from `dplyr::group_by()`
   - `plot_scores()` now support objects of class `waas_mean`.
   - Include inst/CITATION to return a reference paper with `citation("metan")`.
   - Change 'PC2' with 'PC1' in y-axis of `plot_scores(type = 2)` ([#1](https://github.com/TiagoOlivoto/metan/issues/1))
   - `get_model_data()` now support models of class `anova_joint` and `gafem` and extract random effects of models fitted with `waasb()` and `gamem()`.
   - Update `plot.waasb()` and `plot.gamem()` to show distribution of random effects.
   - `inspect()`, `cv_blup()`, `cv_ammif()`, and `cv_ammi()` now generate a warning message saying that is not possible to compute cross-validation procedures in experiments with two replicates only. Thanks to [@Vlatko](https://www.researchgate.net/profile/Vlatko-Galic) for his email.
   - `plot.wsmp()` now returns heatmaps created with ggplot2. Thus, we removed dependency on `gplots`.
   - Vignettes updated

# metan 1.2.1
* References describing the methods implemented in the package were included in description field of DESCRIPTION file as suggested by the CRAN team.

# metan 1.2.0
* Minor changes
   * `corr_plot()` now don't write a warning message to the console by default.
   * `select_numeric_cols()` now is used as a helper function in `metan`.
   * `metan` now reexports `mutate()` from `dplyr` package.
   * `get_model_data()` now set default values for each class of models.
   * Argument `by` that calls internally `split_factors()` included to facilitate the application of the functions to each level of one grouping variable.

* New functions
   * `add_cols()`, and `add_rows()` for adding columns and rows, respectively.
   * `remove_cols()`, and `remove_rows()` for removing columns and rows, respectively.
   * `select_cols()` and `select_rows()` for selecting columns and rows, respectively.
   * `select_numeric_cols()`, and `select_non_numeric_cols()` for selecting numeric and non-numeric variables quickly.
   * `round_cols()` for rounding a whole data frame to significant figures.
   * `all_lower_case()`, and `all_upper_case()` for handling with cases.
   * `extract_number()`, `extract_string()`, `remove_strings()`, `replace_number()`, and  `replace_string()`, for handling with numbers and strings.
   * `get_level_size()`, and `get_levels()` for getting size of levels and levels of a factor.
   * `means_by()` for computing means by one or more factors quickly.
   * `ge_means()` for computing genotype-environment means
   * `ge_winners()` for getting winner genotypes or ranking genotypes within environments.
   * `env_dissimilarity()` for computing dissimilarity between test environments.

# metan 1.1.2

* Reexport select_helpers `starts_with()`, `ends_with()`, `contains()`, `contains()`, `num_range()`, `one_of()`, `everything()`, and `last_col()`.
* When possible, argument `resp` (response variable(s) now support select helpers.
* New helper function `sem()` for computing standard error of mean.
* New helper functions `remove_rows_na()` and `remove_cols_na()` for removing rows or columns with `NA` values quickly.
* New select helpers `difference_var()`, `intersect_var()`, and 
`union_var()` for selecting variables that match an expression.
* New function `Schmildt()` for stability analysis.
* Plot regression slope and mean performance in objects of class `ge_reg`.
* Update `get_model_data()` to support objects of class `Schmildt`and `Annicchiarico`.


# metan 1.1.1

* Now `on.exit()` is used in S3 generic functions `print()` to ensure that the settings are reset when a function is excited.
* Computationally intensive parts in vignettes uses pre-computed results.

# metan 1.1.0
I'm very pleased to announce the release of `metan` 1.1.0, This is a minor release with bug fixes and new functions. The most important changes are described below.

* New function `corr_stab_ind()` for computing Spearman's rank correlation between stability indexes;
* New function `corr_coef()` for computing correlation coefficients and p-values;
* New S3 method `plot.corr_coef()` for creating correlation heat maps;
* New S3 method `print.corr_coef()` for printing correlation and p-values;
* New helper functions `make_lower_tri()` and `make_upper_tri()` for creating lower and upper triangular matrices, respectively.
* New helper function `reorder_cormat()` for reordering a correlation matrix according to the correlation coefficients;
* Improve usability of `get_model_data()` by supporting new classes of models. Now, `get_model_data()` can be used to get all statistics or ranks computed with the wrapper function `ge_stats()`.
* `arrange_ggplot()` now support objects of class `ggmatrix`.
* Change the default plot theme to `theme_metan()`
* Update function's documentation;
* Update vignettes.

# metan 1.0.2
* New function `arrange_ggplot()` for arranging ggplot2 graphics;
* New function `ge_effects()` for computing genotype-environment effects;
* New function `gai()` for computing the geometric adaptability index;
* New helper function `gm_mean()` for computing geometric mean;
* New helper function `hm_mean()` for computing harmonic mean;
* New helper function `Huehn()` for computing Huehn's stability statistic;
* New helper function `Thennasaru()` for computing Thennasaru's stability statistic;
* Improve usability of `get_model_data()` by supporting new classes of models;
* Update function's documentation;
* Update vignettes;

# metan 1.0.1
* New function `gamem()` for analyzing genotypes in one-way trials using mixed-effect models;
* New function `desc_wider()` to convert an output of the function `desc_stat()` to a 'wide' format;
* New function `Fox()` for Stability analysis;
* New function `Shukla()` for stability analysis;
* New function `to_factor()` to quickly convert variables to factors;
* Improve usability of `get_model_data()` function;
* Update function's documentation;
* Update vignettes;

# metan 1.0.0
The changes in this version were made based on suggestions received when metan was submitted to CRAN for the first time.

## Major changes
The documentation of the following functions was updated by including/updating the \\value section of .Rd files.

* `AMMI_indexes()`
* `Annichiarico()`
* `anova_ind()`
* `as.lpcor()`
* `as.split_factors()`
* `bind_cv()`
* `can_cor()`
* `comb_vars()`
* `corr_ci()`
* `corr_plot()`
* `covcor_design()`
* `cv_ammi()`
* `cv_ammif()`
* `cv_blup()`
* `desc_stat()`
* `ecovalence()`
* `fai_blup()`
* `ge_factanal()`
* `ge_plot()`
* `ge_reg()`
* `ge_stats()`
* `get_model_data()`
* `is.lpcorr()`
* `is.split_factors()`
* `mahala()`
* `mahala_design()`
* `make_mat()`
* `make_sym()`
* `mtsi()`
* `pairs_mantel()`
* `plot.*()` and `plot_*()` functions
* `rbind_fill()`
* `resca()`
* `resp_surf()` 
* `waas()`
* `wsmp()`
* `waasb()`
      
## Minor changes

To allow automatic testing, the examples of the following functions were unwrapped by deleting \\dontrun{}.

* `bind_cv()`
* `clustering()`
* `comb_vars()`
* `corr_ci()`
* `corr_plot()`
* `covcor_design()`
* `desc_stat()`
* `ecovalence()`
* `path_coefff()`
* `plot.fai_blup()`
* `plot.mtsi()`
* `plot.wsmp()`
* `plot_ci()`
* `wsmp()`

In the examples of the functions for cross-validation \\dontrun{} was changed with \\donttest{}
* `cv_ammi()`
* `cv_ammif()`
* `cv_blup()`
* `plot.cv_ammif()`
      
# metan 0.2.0
This is the first version that will be submitted to CRAN. In this version, deprecated functions in the last versions were defunct. Some new features were implemented.

* New functions
   * `fai_blup()` computes the FAI-BLUP index (https://onlinelibrary.wiley.com/doi/full/10.1111/gcbb.12443)
   * `gge()` computes the genotype plus genotype-vs-environment model.
   * `plot_factbars()` and `plot_factlines()` are used to create bar and line plots, respectively, considering an one- or two-way experimental design.
   * `desc_stat()` computes several descriptive statistics.
   * `can_corr()`computes canonical correlation coefficients.
   * `resp_surf()` computes response surface model using two quantitative factors.
   * `make_mat()` is used to create a two-way table using two columns (factors) and one response variable. 
   * `make_sym()` is used to create a symmetric matrix using a upper- or lower-diagonal matrix. 
   
* Minor improvements
   * New evaluation for text vectors are now used in the functions `AMMI_indexes()` and `fai_blup()` and `desc_stat()`. For example, to indicate the statistics to be computed in `desc_stat()` you must use now ` stats = c("mean, SE.mean, CV, max, min"))` instead  `stats = c("mean", "SE.mean", "CV", "max", "min"))`

# metan 0.1.5
In the latest development version, the package **METAAB** was renamed to **metan** (**m**ulti-**e**nvironment **t**rials **an**alysis). Aiming at a cleaner coding, in this version, some functions were deprecated and will be defunct in the near future. Alternative functions were implemented.

* For `WAAS.AMMI()`, use `waas()`.
* For `WAASBYratio()`, use `wsmp()`.
* For `WAASratio.AMMI()`, use `wsmp()`.
* For `autoplot.WAAS.AMMI()`, use `autoplot.waas()`.
* For `plot.WAASBYratio()`, use `plot.wsmp()`.
* For `plot.WAASratio.AMMI()`, use `plot.wsmp()`.
* For `predict.WAAS.AMMI()`, use `predict.waas()`.
* For `summary.WAAS.AMMI()`, use `summary.waas()`

Widely-known parametric and nonparametric methods were implemented, using the following functions.

* `Annicchiarico()` to compute the genotypic confidence index.
* `ecovalence()` to compute the Wricke's ecovalence.
* `ge_factanal()` to compute to compute the stability and environmental.
* `ge_reg()` to compute the joint-regression analysis.
stratification using factor analysis.
* `superiority()` to compute the nonparametric superiority index.


# METAAB 0.1.4
In the latest development version, some useful functions were included. One of the most interesting features included in this version was allowing the functions to receive data from the forward-pipe operator %>%. Bellow are the functions included in this version.

* `anova_ind()` to perform a within-environment analysis of variance easily;
* `colindiag()` to perform a collinearity diagnostic of a set of predictors;a
* `find_outliers()` to easily find possible outliers in the dataset;
* `group_factors()` to split a dataset into a list of subsets using one or more grouping factors. This function may be used befor some functions, e.g., `find_outliers()`, `colindiag()`, `path_coeff()` to compute the statistics for each level of the factor (or combination of levels of factors).
* `lpcor()` to compute linear and partial correlation coefficients.
* `pairs_mantel()` to compute a graphic pairwise Mantel's test using a set of correlation matrices;
* `path_coeff()` to compute path coefficients with minimal multicollinearity;

The following S3 Methods were also implemented:

* `is.group_factors()` and `as.group_factors()` to check or easily coerce a dataframe that has one or more factor columns to an object of `group_factors`;
* `is.lpcorr()` and `as.lpcorr()`  to check or easily coerce a list of correlation matrices to an object of `lpcorr`;


# METAAB 0.1.3
* AMMI-based stability indexes;
* Allow analyzing multiple variables at the same time;
* S3 methods such as `plot()`, `predict()`, `summary()` implemented.

# METAAB 0.1.2

* Mixed-effect model with environment random effect;
* Random-effect model.

# METAAB 0.1.1

* The first version of the package
