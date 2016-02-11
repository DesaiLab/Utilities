FUNCTION bytebin,byt
;convert number into binary, pad to make at least 8 bits
  num = strtrim(string(byt,format='(b-32.8)'),2)
  return, byte(fix(strmid(num,indgen(strlen(num)),replicate(1,strlen(num)))))
END
