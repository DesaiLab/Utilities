FUNCTION isleap,yr
  return,((((Yr mod 4) eq 0) and ((yr mod 100) ne 0)) or ((yr mod 400) eq 0))
END
