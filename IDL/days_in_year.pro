FUNCTION days_in_year,yr
  IF isleap(yr) THEN return,366 ELSE return,365
END
