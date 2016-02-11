FUNCTION stderror,observed,predicted
  on_error, 2
  return, sqrt((total((observed-predicted)^2))/n_elements(observed))
END
