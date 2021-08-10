# r-shiny-fair-risk
Shiny application that can estimate risk using factor analysis for information risk (FAIR) risk analysis methods.

Mostly leverages lognormal distributions for calculating various distributions to reflect the possiblility for the rare high likelihood and/or impact event. This selection was influenced by the book How to _Measure Anything in Cybersecurity Risk_ by Douglas Hubbard. It also simplifies the range estimates by only requiring the estimation of two values instead of three and a confidence. It also avoids having to account for the extreme low and high (black swans) probability events.

Distributions are used to calculate the various ranges of possible outcomes and are then combined to determine a final distribution of loss events.

The model provides three sets of results:
  - Inherent Loss Event Frequency (LEF)/Likelihood, Loss Magnitude (Loss)/Impact and Risk (1 and 10 year)
  - Current LEF/Residual Likelihood, Loss/Impact and Risk (1 and 10 year)
  - Future Residual LEF/Likelihood, Loss/Impact and Risk (1 and 10 year)

Within the application you can select to determine the LEF/likelihood range directly or indirectly, via threat event frequency (TEF), threat capability (TCAP), and resistance (control) strength (RS).
  - You may also determine Likelihood using TEF and vulnerability (VULN), without the use of TCAP and RS.

The inherent risk results are not provided when evaluating Likelihood directly due to the inherent risk being a product of:
  - Increasing the TEF by 20% (assumes increased threat activity in the absences of controls)
  - Setting the VULN percentage to 95% (assumes there are some controls (5%) that are outside of the control of the organization)

These values were set somewhat arbitrarily, but the 95% VULN percentage is equivelent to a TCAP of 1% - 99% and a RS of 1% - 1%.

To use the Shiny applications you will need:
  - Rstudio (so far have only tested with Version 1.3.1093)
  - R (so far have only tested with Version 4.0.4)

Open the app.R file in RStudio and click the Run App button in the top right corner of the code editing pane.

The application is written to be reactive, therefore values will update dynamically as other values are changed.
