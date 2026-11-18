
is_nullna = function(x){
  #' Tests if input is NULL or NA
  #'
  #' @param x object to be tested, if a vector then each is tested
  #' @return logical vector 
  
  # if the input has no length then of course is is NULL or empty (the
  # same for our purposes)
  if (length(x) == 0) return(TRUE)
  
  sapply(x, 
         function(x){
           r = is.null(x)
           if (!r) r = any(is.na(x))
           r
         })
}