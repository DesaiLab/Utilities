FUNCTION prop_test,ar,corrected=corrected
;Test porportions of each row , generate chi square and p value for
;each

  coltot = total(ar,2)
  output = make_array(2,n_elementS(ar[0,*]),/float)  
  FOR i = 0,n_elements(ar[0,*])-1 DO BEGIN
    row1 = ar[*,i]
    row2 = coltot - row1
    output[*,i] = cti_test([[row1],[row2]],corrected=corrected)
  ENDFOR 
  return,output
END
