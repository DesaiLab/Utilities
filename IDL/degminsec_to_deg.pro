FUNCTION DegMinSec_To_Deg, deg, min, sec
  Return, Deg + (Min / 60.0) + (Sec / 3600.0)
END
