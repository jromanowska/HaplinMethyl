# HaplinMethyl

[![DOI](https://zenodo.org/badge/350686987.svg)](https://zenodo.org/doi/10.5281/zenodo.5554602)

Additional package to extend the data read by [Haplin](https://cran.r-project.org/package=Haplin).

The HaplinMethyl package provides three categories of important functions:

- reading the data,
- loading the read-in data, and
- manipulating the environmental data.

## For those who can't wait to try things out...

### Reading the environmental data:

The two functions (`envDataRead` and `envDataReadFromObj`) read in
the data and create an object of `env.data` class, which is
a list of ff-matrices. Simultaneously, two files are written: `.RData`
and `.ffData`. These are connected and will be used in the data-loading
function. The object created here has also `summary` and `print`
methods defined for easier viewing.

### Loading the read-in data:

`envDataLoad` function loads the data, which is super-fast, compared to
reading the data in.

### Manipulating the data:

The user can extract a subset of the data read in by any of the above
functions, with `envDataSubset`. This will create new `.RData` and
`.ffData` files. Moreover, there are two functions that are created
specifically for DNA methylation data: `findCpGsnearSNP` and
`findCpGsRange` can be used to create CpG lists that are near a given
SNP or within a certain coordinate range.

## Read more...

Checkout the [vignettes](articles/index.html)!

```{r}
vignette(package = "HaplinMethyl")
```

## Installation

If you don't have `devtools` installed, run

```{r}
install.packages("devtools")
```

Then, you're ready to install the newest HaplinMethyl via

```{r}
devtools::install_github("jromanowska/HaplinMethyl")
```

