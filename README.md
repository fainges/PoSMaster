###README####

PoSMaster is a [Shiny](https://shiny.rstudio.com/) app written in R. It should be pretty much self contained, but you'll need to install all the packages listed below,
and you'll need the Decred network daemon, dcrd, running along with [dcrdata](https://github.com/dcrdata/dcrdata).

You'll need to change lines 10 and 13 in getData.R to point to these programes.

##Required R Packages##

You'll need the following packages installed using the R command, ```install.packages```:

* shiny
* tidyr
* dplyr
* DT
* lubridate
* jsonlite
* data.table
* xts
* ggplot2
* quantmod