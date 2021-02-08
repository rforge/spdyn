print.moranPerm <- function(x, ...){
  cat('The value of the moran statistic is: ',x$statistic,
  '\nThe pseudo p-value is: ', x$pvalue,
  '\nNumber of simulations: ', length(x$sim_statistics)-1
  )
}
