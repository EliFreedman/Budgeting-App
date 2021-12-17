library(tidyverse)

federalTax <- readr::read_csv('Federal Tax Rates-Brackets.csv')

federalTaxCalculator <- function(annualIncome) {
  # Determines what tax bracket `annualIncome` falls in
  for(row in 1:nrow(federalTax)) {
    # If `annualIncome` is the top bracket
    if(row == nrow(federalTax)) {
      federalTax <- federalTax[row, 1]
      break
    }
    # If `annualIncome` is less than the next bracket in the vector, stop
    if(annualIncome < federalTax[row + 1, 2]) {
      federalTax <- federalTax[row, 1]
      break
    }
  }
  
  return(as.integer(annualIncome - ((100 - federalTax) / 100) * annualIncome))
}