FUNCTION JD_to_DY, jda, yr = yr
  if n_elements(yr) EQ 0 then yr = '97'
  output = strarr(n_elements(jda))

  FOR jj = 0l,n_elements(jda)-1l DO BEGIN
    jd = jda[jj]
    IF n_elements(yr) GT 1 THEN year = yr[jj] ELSE year=yr[0]
    md = [31,28,31,30,31,30,31,31,30,31,30,31]
    IF isleap(fix(year)) THEN md[1] = 29
    modcum = fix(total(md,/cumulative))
    modcum = fix(jd)-(modcum - md)
    l = where(modcum le 0)
    if l[0] ne -1 then modcum[l] = 999

    dy = min(modcum)
    mo = (where(modcum eq dy))[0] + 1

    year = strtrim(fix(year),2)
    mo = string(mo,format='(i2.2)')
    dy = string(dy,format='(i2.2)')
    output[jj] = year+mo+dy
  ENDFOR 

  IF n_elements(output) EQ 1 THEN output = output[0]
  
  return, output
END
