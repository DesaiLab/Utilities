FUNCTION binbyte,bits
;convert bits into a number
  return,long(total(reverse(bits)*(2l^lindgen(n_elements(bits))),/integer))
END 
