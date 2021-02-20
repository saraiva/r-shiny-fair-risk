# r-shiny-fair-risk
Shiny application that can estimate risk using factor analysis for information risk (FAIR) risk analysis methods.

Leveraged lognormal distributions for calculating various distributions to reflect the possiblility for the rare high likelihood and/or impact event.

Distributions are used to calculate the various ranges of possible outcomes and are then combined to determine a final distribution of loss events.

The model provides two result:
  - Inherent Likelihood, Impact and Risk
  - Residual Likelihood, Impact and Risk


Within the application you can select to determine the likelihood range directly or indirectly, via threat event frequency, threat capability, and resistance (control) strength.
  - You may also determine likelihood using threat event frequency and vulnerability, without the use of threat capability and resistence (control) strength

The inherent risk results are not provided when evaluating likelihood directly due to the inherent risk being a product of:
  - Increasing the threat event frequency by 20% (assumes increased threat activity in the absences of controls)
  - Setting the vulnerability percentage to 95% (assumes there are some controls (5%) that are outside of the control of the organization)
These values were set somewhat arbitrarily, but the 95% vulnerability percentage is equivelent to a threat capability of 1% - 99% and a control strength of 1% - 1%.
