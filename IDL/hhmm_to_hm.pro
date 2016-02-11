FUNCTION hhmm_to_hm,hhmm
  goodhhmm = string(hhmm,format='(i4.4)')
  goodhh = strmid(goodhhmm,0,2)
  goodmm = strmid(goodhhmm,2,2)
  return,[transpose([fix(goodhh)]),transpose([fix(goodmm)])]
END
