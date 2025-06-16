

lambdafunction = function(TPR,lambdamin) {
  ## equation 4 from paper
### transformation describing adjustment for malaria-attributable proportion
                1+(lambdamin - 1)*TPR
}
 
### primary corrected incidence calculations
 
correct_incidence = function(D, E, F, pop, beta, alpha, standardincidence,lambdamin) {
                # D is test negatives
                # E is test positives
                # F is non-tested
                TPR_nontested = alpha * E/(D+E)
                B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
                C = TPR_nontested / (1-TPR_nontested) * B
                A = F - B - C
                if (pop == 0 | is.na(pop)) { pop = 1 }
 
                lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
                nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/pop*1000
 
#             nonmalariafever_incidence = (B+D)/pop*1000
                corrected_incidence = (C+E)/pop*(standardincidence/nonmalariafever_incidence)*1000 # without attributable correction
                corrected_incidence2 = (C+E)*lambda/pop*(standardincidence/nonmalariafever_incidence)*1000 # with attributable correction
 
                testing_correction = (C+E)/E
                incidence_correction = standardincidence/nonmalariafever_incidence
                c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}
 
# alpha_estimate = 0.475
# beta_estimate = 0.589
# lambdamin_median = 0.75
# standardincidence_estimate = 10000
#  
# corrected_incidence_bydistrict = sapply(1:dim(data)[1], function (x)
#                                                                                                                 
# correct_incidence(data$cas_testes[x] - data$cas_confirmes[x], data$cas_confirmes[x], 
#                   data$consultation_toutes_cause_confondues[x] - data$cas_testes[x],
#                   data$population[x], beta_estimate, alpha_estimate, standardincidence_estimate,lambdamin_median)[1]
# )

 
adjCorrInc = function( 
    allCauseAttendance, 
    numberTested , 
    confirmedMalaria , 
    expectedNonMalariaFeverRate = 0.589 ){
  
  # return null if values don't make sense
  if ( is.na( confirmedMalaria ) | is.na( numberTested ) | is.na( allCauseAttendance ) ) return(NA)
  if ( (confirmedMalaria  >= numberTested) | (numberTested >= allCauseAttendance) ) return(NA)
  
  reportedNonMalariaFeverRate = ( numberTested - confirmedMalaria ) / ( allCauseAttendance - confirmedMalaria )

  expectedNonMalariaFever = expectedNonMalariaFeverRate * ( allCauseAttendance  )
  
  unTestedNonMalariaFever = expectedNonMalariaFever - numberTested 
  
  reportedMalariaRate = confirmedMalaria / allCauseAttendance 
  
  tpr = confirmedMalaria / numberTested
  
  # if more suspected fevers than expected, return actual malaria rate
  if ( reportedNonMalariaFeverRate >= expectedNonMalariaFeverRate ) return( reportedMalariaRate  )
  
  #Otherwise, add expected cases from untested patients
  adjMalariaRate = ( confirmedMalaria +  unTestedNonMalariaFever * tpr ) / allCauseAttendance 
  
  
  return( adjMalariaRate ) 
}                      

adjCorrCases = function( 
    allCauseAttendance, 
    numberTested , 
    confirmedMalaria , 
    expectedNonMalariaFeverRate = 0.589 ){
  
  # return null if values don't make sense
  if ( any( is.na( confirmedMalaria ) , is.na( numberTested ) , is.na( allCauseAttendance ) ) ) return(NA)
  if ( any(confirmedMalaria  >= numberTested , numberTested >= allCauseAttendance  ) ) return(NA)
  
  reportedNonMalariaFeverRate = ( numberTested - confirmedMalaria ) / ( allCauseAttendance - confirmedMalaria )

  expectedNonMalariaFever = expectedNonMalariaFeverRate * ( allCauseAttendance  )
  
  unTestedNonMalariaFever = expectedNonMalariaFever - numberTested 
  
  reportedMalariaRate = confirmedMalaria / allCauseAttendance 
  
  tpr = confirmedMalaria / numberTested
  
  # if more suspected fevers than expected, return actual malaria rate
  if ( reportedNonMalariaFeverRate >= expectedNonMalariaFeverRate ) return( confirmedMalaria  )
  
  #Otherwise, add expected cases from untested patients
  adjMalariaCases = confirmedMalaria +  unTestedNonMalariaFever * tpr 
  
  
  return( round( adjMalariaCases ) )
}                                                                                                                                                              


# Test
# library( tidyverse )
# x = tibble(
#   allCauseAttendance = rep( 1000, 7 ) ,
#   numberTested = c( 55, 100, 200 , 400, 500 , 600 , 700) ,
#   confirmedMalaria = rep( 50 , 7 )
# ) %>%
#   rowwise() %>%
#   mutate(
#     incidence = 1000 * confirmedMalaria / allCauseAttendance  ,
#     aci = adjCorrInc(  allCauseAttendance , numberTested , confirmedMalaria  )
#     )
# x
