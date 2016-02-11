FUNCTION CST_to_UTC, cst, cdt = cdt
  if keyword_set(cdt) then inky = 5.0 else inky = 6.0
  return, cst + inky
END
