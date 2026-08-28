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

`welfare_state_funct` is a four-digit string (each digit between 1 and 9),
one digit per welfare dimension, in this order: **Nutrition**,
**Environment**, **Health**, **Behavioral Interactions**. Each digit
corresponds to one of the following levels:

* 1 - High-level positive experiences
* 2 - Significant positive experiences
* 3 - Mild positive experiences
* 4 - Minor positive experiences
* 5 - Neutral: negative and positive experiences compensate
* 6 - Minor negative experiences
* 7 - Mild negative experiences
* 8 - Severe negative experiences
* 9 - Very severe negative experiences

`number_years_funct` is the duration spent in that welfare state by the
animal, expressed in number of years (e.g. `30 / 365.25` for 30 days).

### Examples

The welfare of an animal who has "Mild negative experiences" for Nutrition
and Health, and "Very severe negative experiences" for Environment and
Behavioral Interactions, during 30 days:

```r
AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365.25)
```

The welfare change when Nutrition improves to Neutral:

```r
AQALYscore(welfare_state_funct = "5979", number_years_funct = 30 / 365.25) -
  AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365.25)
```

The welfare change when Nutrition improves to Neutral and lifetime
increases to 45 days:

```r
AQALYscore(welfare_state_funct = "5979", number_years_funct = 45 / 365.25) -
  AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365.25)
```

## Data

The package ships with `estimatesAQALY`, the mixed logit estimation
results (coefficients and variance-covariance matrix) fitted on the
combined UK replicate data. See `?estimatesAQALY` for details, and see
the associated paper for the estimation procedure.
