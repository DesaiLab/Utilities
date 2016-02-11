FUNCTION clip_array,arr,locs=locs,badval=badval
  IF keyword_set(badval) THEN arrrange = where(finite(arr) AND (arr NE badval),numrange) ELSE arrrange = where(finite(arr),numrange)
  IF numrange GT 0 THEN BEGIN 
    startloc = arrrange[0]
    endloc = arrrange[numrange-1]
    locs = [startloc,endloc]
    return,arr[startloc:endloc]
  ENDIF ELSE BEGIN
    locs = [0,0]
    return,[nan()]
  ENDELSE
END
