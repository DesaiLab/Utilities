FUNCTION UTC_to_CST, utc, cdt=cdt
  if keyword_set(cdt) then inkd = -5.0 else inkd = -6.0
  return, utc + inkd
END
