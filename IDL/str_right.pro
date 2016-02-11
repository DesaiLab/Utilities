FUNCTION str_right,str,val
  return,strmid(str,val-1,/reverse_offset)
END
