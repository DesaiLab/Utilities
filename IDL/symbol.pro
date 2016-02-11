FUNCTION symbol,type,size=size,color=color,thick=thick,fill=fill,connect=connect
  IF n_elements(size) EQ 0 THEN size = 2.0
  IF n_elements(type) EQ 0 THEN type = 0
  tp = size(type,/type)
  IF tp EQ 7 THEN BEGIN
    CASE strupcase(type) OF 
      'CIRCLE' : sym = circle(size)
      'SQUARE' : sym = square(size)
      'TRIANGLE' : sym = triangle(size)
      'DIAMOND' : sym = diamond(size)
      'STAR' : sym = star(size)
      ELSE : sym = circle(size)
    ENDCASE 
  ENDIF ELSE BEGIN
    CASE type OF
      0 : sym = circle(size/2.0)
      1 : sym = square(size/1.5)
      2 : sym = triangle(size)
      3 : sym = diamond(size)
      4 : sym = star(size)
      ELSE : sym = circle(size/2.0)
    ENDCASE 
  ENDELSE 
  usersym,sym,color=color,thick=thick,fill=fill
  IF keyword_set(connect) THEN return,(-8) ELSE return,8
  return,8
END
