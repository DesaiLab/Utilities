FUNCTION mysmooth,d,w,back=back,forward=forward,_extra=ex

  IF w MOD 2 EQ 0 THEN wid = w+1 ELSE wid = w

  IF keyword_set(back) THEN BEGIN
    pad = [replicate(d[0],wid-1),reform(d)]
  ENDIF ELSE BEGIN 
    IF keyword_set(forward) THEN BEGIN
      pad = [reform(d),replicate(d[n_elements(d)-1],wid-1)]
    ENDIF ELSE BEGIN
      pad = [replicate(d[0],(wid-1)/2),reform(d),replicate(d[n_elements(d)-1],(wid-1)/2)]
    ENDELSE
  ENDELSE 

  output = d

  FOR i = 0,wid-1 DO output[i:*:wid] = average_arr(pad[i:*],wid,_extra=ex)
  return,output
END
