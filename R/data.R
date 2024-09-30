#' Example demography data used for Figure 2
#'
#' @format ## `demography`
#' A data frame with 48 rows and 5 columns:
#' \describe{
#'   \item{X}{Record ID number}
#'   \item{Age}{Age in years}
#'   \item{Type}{Diabetis Type A/B}
#'   \item{Gender}{Gender Male/Female}
#'   \item{Prevalence}{Prevalence}
#'
#' }
"demography"

#' Example bands data used for Figure 3
#'
#' @format ## `data_bands`
#' A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{level}{level each band represents}
#'   \item{ystart}{starting point of each band}
#'   \item{yend}{ending point of each band}
#'   \item{col}{color of each band}
#'
#' }
"data_bands"

#' 1) stage: stage each line represents
#' 2) xstart: x-axis starting point of each line
#' 3) xend: x-axis ending point of each line
#' 4) y: y-axis position of each line
#' 5) col: color of each line
#' 6) xpos: x-axis position of each annotation (relative to xstart)
#' 7) ypos: y-axis position of each annotation (relative to y)
#'
#' Example lines data used for Figure 3
#'
#' @format ## `data_lines`
#' A data frame with 7 rows and 7 columns:
#' \describe{
#'   \item{stage}{stage each line represents}
#'   \item{xstart}{x-axis starting point of each line}
#'   \item{xend}{x-axis ending point of each line}
#'   \item{y}{y-axis position of each line}
#'   \item{col}{color of each band}
#'   \item{xpos}{x-axis position of each annotation (relative to xstart)}
#'   \item{ypos}{y-axis position of each annotation (relative to y)}
#'
#' }
"data_lines"

#' Example comorbidities data used for Figure 4
#'
#' @name comorbidities
#' @format A data frame with 15 rows and 3 variables
#' \describe{
#'   \item{Comorbidities}{Comorbidities}
#'   \item{Severity}{Severity of comorbidities Mild/Moderate/Severe}
#'   \item{Prevalence}{Prevalence of comorbidities of each severity level}
#'
#' }
"comorbidities"

#' Example effects table data used for Figure 6 and Figure 7
#'
#' @name effects_table
#' @format A data frame with 24 rows and 51 variables
"effects_table"

#' Example effects table
#'
#' @name brdata
#' @format A data frame with 105 rows and 51 variables
"brdata"

#' Example correlogram data used for Figure 10
#'
#' @name corr
#' @format A data frame with 100 rows and 6 columns:
#'   \describe{
#'   \item{Primary Efficacy}{Simulated change from primary efficacy baseline in
#'   respective subject}
#'   \item{Secondary Efficacy}{Simulated change from secondary efficacy baseline
#'   in respective subject}
#'   \item{Quality of Life}{Simulated change from quality of life baseline
#'   in respective subject}
#'   \item{Recurring AE}{Simulated change from recurring AE baseline
#'   in respective subject}
#'   \item{Rare SAE}{Simulated change from rare SAE baseline
#'   in respective subject}
#'   \item{Liver Toxicity}{Simulated change from liver toxicity baseline
#'   in respective subject}
#'
#'   }
"corr"

#' Example composite outcome data used for Figure 12
#'
#' @name comp_outcome
#' @format A data frame with 1800 rows and 6 variables
#' \describe{
#'   \item{usubjid}{Subject ID}
#'   \item{visit}{Visit}
#'   \item{trtn}{Treatment arms in numeric type}
#'   \item{trt}{Treatment arms in character type}
#'   \item{brcatn}{Category of composite outcome in numeric type}
#'   \item{brcat}{Category of composite outcome in character type}
#'
#' }
"comp_outcome"

#' Example scatterplot data used for Figure 11
#'
#' @name scatterplot
#' @format A data frame with 500 rows and 2 columns:
#'   \describe{
#'   \item{bdiff}{Simulated difference in incremental probabilities for
#'   active/control effects and outcome "Benefit"}
#'   \item{rdiff}{Simulated difference in incremental probabilities for
#'   active/control effects and outcome "Risk"}
#'
#'   }
"scatterplot"

#' Example cumulative excess plot data used for Figure 13
#'
#' @name cumexcess
#' @format A data frame with 880 rows and 9 columns:
#'   \describe{
#'   \item{eventtime}{Simulated event times}
#'   \item{diff}{Simulated difference in active/control effects}
#'   \item{obsv_duration}{Duration of observational period}
#'   \item{obsv_unit}{Unit length of observational period}
#'   \item{outcome}{Specifies Benefit/Risk}
#'   \item{eff_diff_lbl}{Label for effect difference}
#'   \item{n}{Number of subjects}
#'   \item{effect}{Specifies active/control effect}
#'   \item{eff_code}{0/1 depicting control/active effects}
#'
#'   }
"cumexcess"

#' Example decision_table used for decision table
#'
#' @format ## `decision_table`
#' A data frame with 48 rows and 5 columns:
#' \describe{
#'   \item{Type of Outcome}{Type of Outcome}
#'   \item{Summary Statistics for Primary Outcome}{Summary Statistics for Primary Outcome}
#'   \item{Summary Statistics for Treatment Outcome}{Summary Statistics for Treatment Outcome}
#'   \item{Method for Populating CI}{Method for Populating CI}
#'   \item{Required Columns}{Required Columns}
#'
#' }
"decision_table"
