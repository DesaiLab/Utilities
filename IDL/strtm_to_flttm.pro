FUNCTION StrTM_to_FltTM,strtm,nosec=nosec
  hr = strmid(strtm,0,2)
  min = strmid(strtm,2,2)
  if not keyword_set(nosec) then sec = strmid(strtm,4,2) else sec = replicate('00',n_elements(hr))
  hr = float(hr)
  min = float(min)
  sec = float(sec)
  return, hr + (min / 60.0) + (sec / 3600.0)
END
