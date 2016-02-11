FUNCTION merge_array,arr1,arr2,arr3,arr4,arr5,arr6,arr7,arr8,arr9,filled=filled,intfill=intfill,lgarr=lgarr
  IF n_elements(arr1) NE 0 THEN outarr = arr1 ELSE return,nan()
  IF NOT keyword_set(intfill) THEN intfill = replicate(1,n_elements(outarr))
  bad = where(~finite(outarr),nbad)
  IF n_elements(lgarr) NE 0 THEN BEGIN
    IF (size(lgarr))[0] EQ 2 THEN BEGIN 
      arr2 = reform(lgarr[0,*])
      IF (size(lgarr))[1] GT 1 THEN nlgarr = lgarr[1:*,*]
    ENDIF 
  ENDIF 
  IF (n_elements(arr2) EQ n_elements(arr1)) AND (nbad GT 0) THEN BEGIN 
    outarr[bad] = arr2[bad]
    intfill[bad] = intfill[bad]+1
    return,merge_array(outarr,arr3,arr4,arr5,arr6,arr7,arr8,arr9,intfill=intfill,filled=filled,lgarr=nlgarr)
  ENDIF ELSE BEGIN 
    IF (nbad GT 0) THEN intfill[bad] = 0
    filled = intfill
    return,outarr
  ENDELSE 
END
