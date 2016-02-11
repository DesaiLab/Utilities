FUNCTION str_strip,str,val
  IF NOT keyword_set(val) THEN val = 1
  return,strmid(str,0,strlen(str)-val)
END
