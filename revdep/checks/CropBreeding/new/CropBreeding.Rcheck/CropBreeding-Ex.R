pkgname <- "CropBreeding"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('CropBreeding')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("breeding_metrics")
### * breeding_metrics

flush(stderr()); flush(stdout())

### Name: breeding_metrics
### Title: Breeding Metrics Calculation
### Aliases: breeding_metrics

### ** Examples




cleanEx()
nameEx("gxe_analysis_multiple")
### * gxe_analysis_multiple

flush(stderr()); flush(stdout())

### Name: gxe_analysis_multiple
### Title: Two-Way ANOVA for Genotype x Environment Interaction with
###   Multiple Traits
### Aliases: gxe_analysis_multiple

### ** Examples




cleanEx()
nameEx("perform_ammi_single_trait")
### * perform_ammi_single_trait

flush(stderr()); flush(stdout())

### Name: perform_ammi_single_trait
### Title: Perform AMMI Analysis for a Single Trait
### Aliases: perform_ammi_single_trait

### ** Examples




cleanEx()
nameEx("stability_analysis")
### * stability_analysis

flush(stderr()); flush(stdout())

### Name: stability_analysis
### Title: Stability Analysis using Eberhart-Russell Model
### Aliases: stability_analysis

### ** Examples




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
