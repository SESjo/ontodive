<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

# weanlingNES <img src="man/figures/logo.png" align="right" />

The goal of weanlingNES is to provide in the same place analyses and
functions developed within the framework of my postdoc on the ontogeny
of diving behavior in northern elephant seal ([*Mirounga
angustirostris*](https://en.wikipedia.org/wiki/Northern_elephant_seal)).
Hopefully, it will be helpful for those seeking to pursue this work or
just trying to apply similar analysis to their field. In the near
future, it should also include a bibliography and a draft of a potential
paper.

> `weanlingNES` package does not currently include dataset used for
> analyses, but might be available upon request

## Installation

You can install the released version of weanlingNES from
[GitHub](https://github.com/) with:

    if (require(devtools)) {
      devtools::install_github(repo = "SESjo/weanlingNES", 
                               auth_token = "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")
    } else {
      install.packages("devtools")
      devtools::install_github(repo = "SESjo/weanlingNES", 
                               auth_token = "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")
    }

And the development version with:

    if (require(devtools)) {
      devtools::install_github(repo = "SESjo/weanlingNES", 
                               auth_token = "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", 
                               ref = "develop")
    } else {
      install.packages("devtools")
      devtools::install_github(repo = "SESjo/weanlingNES", 
                               auth_token = "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", 
                               ref = "develop")
    }

> Please note this package is currently not public which is why you can
> not access directly to it at <https://github.com/SESjo/weanlingNES>,
> and that you are required to provide a
> [token](https://en.wikipedia.org/wiki/Access_token) when installing
> it. For convenience, everyone can use the same token (provided above),
> but keep in mind that it should not be that way :)

## Examples

Once installed, you can easily access vignettes (which are supposed to
be associated with a specific analysis) using these commands:

    # load the package
    library(weanlingNES)

    # browse vignette
    browseVignettes("weanlingNES")

The same way you can get information on functions, you can also get
information on dataset used for analyses.

    # get information on dataset used in weanlingNES
    ?data_nes
