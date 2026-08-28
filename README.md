# AQALY

`AQALY` computes Animal Quality-Adjusted Life Year (AQALY) scores from a
four-digit welfare state code and a duration, using mixed logit estimates
from stated-preference data. Standard errors are computed with the delta
method (`msm::deltamethod()`).

## Installation

```r
# install.packages("remotes")
remotes::install_github("EspinosaRomain/AQALY_r")
```

Note: the GitHub repository is named `AQALY_r`, but the installed R package
is still called `AQALY` — after installing, load it as usual with
`library(AQALY)`.

## Usage

```r
library(AQALY)

AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365)
```

## Data

The package ships with `estimatesAQALY`, the mixed logit estimation
results (coefficients and variance-covariance matrix) fitted on the
combined UK replicate data. See `?estimatesAQALY` for details, and see
the associated paper for the estimation procedure.
