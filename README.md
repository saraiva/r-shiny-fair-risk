# r-shiny-fair-risk
Shiny application that can estimate risk using factor analysis of information risk (FAIR) risk analysis methods as defined by The Open Group (https://www.opengroup.org/).

 - Open FAIR Risk Taxonomy (O-RT) - https://publications.opengroup.org/editors-picks/open-fair/c20b
 - Open FAIR Risk Analysis (O-RA) - https://publications.opengroup.org/editors-picks/open-fair/c20a

Mostly leverages lognormal distributions for calculating various distributions in the Monte Carlo simulations to assit in estimating the probability of rare high (black swan) impact events. This selection was influenced by the book _How to Measure Anything in Cybersecurity Risk_ (https://www.howtomeasureanything.com/cybersecurity/) by Douglas Hubbard. It also simplifies the range estimates by only requiring the estimation of two values instead of three and a confidence as required when utilizing PERT distributions. This allow a risk analyst to account for black swan events since the low and high probablity estimates in the application only need to be made with 90% confidence, thereby allowing the risk analyst to avoid having to estimate the bottom and top 5% of cases.

Distributions are used to calculate the various ranges of possible outcomes and are then combined to determine a final distribution of loss events.

The model provides three sets of results:
  - Inherent (before controls) Loss Event Frequency (LEF)/Likelihood, Loss Magnitude (Loss)/Impact and Risk (1 and 10 year)
  - Current Residual (with existing controls) LEF/Likelihood, Loss/Impact and Risk (1 and 10 year)
  - Future Residual (after additional controls) LEF/Likelihood, Loss/Impact and Risk (1 and 10 year)

Within the application you can select to determine the LEF/likelihood range directly or indirectly, via threat event frequency (TEF), threat capability (TCAP), and resistance (control) strength (RS).
  - You may also determine Likelihood using TEF and vulnerability (VULN), without the use of TCAP and RS.

The inherent risk results are not provided when evaluating Likelihood directly due to the inherent risk being a product of:
  - Increasing the TEF by 20% (assumes increased threat activity in the absences of controls)
  - Setting the VULN percentage to 95% (assumes there are some controls (5%) that are outside of the control of the organization)

These values were set somewhat arbitrarily, but the 95% VULN percentage is equivelent to a TCAP of 1% - 99% and a RS of 1% - 1%.

To use the Shiny applications you will need:
  - Rstudio (so far have only tested with Version 1.3.1093) - https://www.rstudio.com
  - R (so far have only tested with Version 4.0.4) - https://cran.rstudio.com/

Open the app.R file in RStudio and click the Run App button in the top right corner of the code editing pane.

The application is written to be reactive, therefore result values will update dynamically as other values are changed.

A demo version of the application can be viewed at SinyApps.io: https://jconnors.shinyapps.io/r-shiny-fair-risk-github/
- Note: this is posted under the free tier so excessive (>10 hours per month) use will result in the application becoming inaccessible.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
