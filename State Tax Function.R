library(tidyverse)

stateTax <- readr::read_csv('State Tax Rates-Brackets.csv')

# Returns the amount of state tax owed
# MUST BE INPUT AS A STRING
stateTaxCalculator <- function(state, annualIncome) {
  # Select the subset of `stateTax` that corresponds to the input `state`
  stateTaxRate <-
    stateTax %>%
    filter(State == state) %>%
    summarize(`Rates (%)`, `Brackets ($)`)
  
  # Determines what tax bracket `annualIncome` falls in
  for(row in 1:nrow(stateTaxRate)) {
    # If there is one/no tax bracket or `annualIncome` is the top bracket
    if(row == nrow(stateTaxRate)) {
      stateTaxRate <- stateTaxRate[row, 1]
      break
    }
    # If `annualIncome` is less than the next bracket in the vector, stop
    if(annualIncome < stateTaxRate[row + 1, 2]) {
      stateTaxRate <- stateTaxRate[row, 1]
      break
    }
  }
  
  return(as.integer(annualIncome - ((100 - stateTaxRate) / 100) * annualIncome))
}