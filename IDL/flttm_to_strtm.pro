FUNCTION FltTM_to_StrTM, ftm, nosec=nosec
  flttm = [ftm]
  hr = fix(flttm)
  rest = (flttm - fix(flttm))*60.0
  min = fix(rest)
  sec = fix((rest - fix(rest))*60.0)
  FOR i = 0,n_elements(flttm)-1 DO if keyword_set(nosec) and (sec[i] ge 30) then min[i] = min[i] + 1

  hr = strtrim(string(hr),2)
  FOR i = 0,n_elements(flttm)-1 DO if strlen(hr[i]) eq 1 then hr[i] = '0' + hr[i]
  min = strtrim(string(min),2)
  FOR i = 0,n_elements(flttm)-1 DO if strlen(min[i]) eq 1 then min[i] = '0' + min[i]
  sec = strtrim(string(sec),2)
  FOR i = 0,n_elements(flttm)-1 DO if strlen(sec[i]) eq 1 then sec[i] = '0' + sec[i]

  IF n_elements(hr) EQ 1 THEN BEGIN
    hr = hr[0]
    min = min[0]
    sec = sec[0]
  ENDIF


  if keyword_set(nosec) then return, strtrim(string(hr)+string(min),2) else return, strtrim(string(hr)+string(min)+string(sec),2)

END
