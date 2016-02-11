FUNCTION Deg_TO_DegMinSec, deg
  D = fix(deg)
  ms = (deg - D) * 60.0
  M = fix(ms)
  S = (ms - M) * 60.0
  Return, [D,M,S]  
END
