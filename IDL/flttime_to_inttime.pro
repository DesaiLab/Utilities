FUNCTION flttime_to_inttime,tm
  t = [tm]
  h = float(fix(t))
  m = float(fix((t-h)*60))
  s = (t-h-(m/60.0))*3600.0
  return,[transpose(h),transpose(m),transpose(s)]
END
