`print.judgeit` <-
function(x,...) {
  if (is.null(x$lambda)) "Object has not undergone preliminary analysis." else
    {writeLines ("Value of object$output:"); print(x$output)}                  
}

