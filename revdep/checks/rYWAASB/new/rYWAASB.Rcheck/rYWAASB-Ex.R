pkgname <- "rYWAASB"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rYWAASB')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PCA_biplot")
### * PCA_biplot

flush(stderr()); flush(stdout())

### Name: PCA_biplot
### Title: The PCA biplot with loadings
### Aliases: PCA_biplot

### ** Examples




cleanEx()
nameEx("bar_plot1")
### * bar_plot1

flush(stderr()); flush(stdout())

### Name: bar_plot1
### Title: The first barplot of the ranks of genotypes
### Aliases: bar_plot1

### ** Examples




cleanEx()
nameEx("bar_plot2")
### * bar_plot2

flush(stderr()); flush(stdout())

### Name: bar_plot2
### Title: The second barplot of the ranks of genotypes
### Aliases: bar_plot2

### ** Examples




cleanEx()
nameEx("data_ge")
### * data_ge

flush(stderr()); flush(stdout())

### Name: data_ge
### Title: Dataset2: a tibble containing ENV, GEN, REP factors and GY(grain
###   yield) and HM agronomic traits from the 'metan' package.
### Aliases: data_ge
### Keywords: datasets

### ** Examples




cleanEx()
nameEx("maize")
### * maize

flush(stderr()); flush(stdout())

### Name: maize
### Title: Dataset1: a tibble containing GEN, Trait, 'WAASB' and 'WAASBY'
###   indexes.
### Aliases: maize
### Keywords: datasets

### ** Examples




cleanEx()
nameEx("nbclust")
### * nbclust

flush(stderr()); flush(stdout())

### Name: nbclust
### Title: Data read and estimate the cluster number
### Aliases: nbclust

### ** Examples




cleanEx()
nameEx("ranki")
### * ranki

flush(stderr()); flush(stdout())

### Name: ranki
### Title: The values and ranks of genotypes
### Aliases: ranki

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
