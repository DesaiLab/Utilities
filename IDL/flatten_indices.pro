FUNCTION flatten_indices,array,dims
;converts multiple dimensions into a multi-dimensional array into a
;single subscript

;dims can be multidimension with each row referring to a different set
;of subscripts into the array
;array can either be the actual array (multidim array), or just the dimensions (1d vector)
  k = size(Array,/dim)
  IF n_elements(k) EQ 1 THEN BEGIN
    IF k[0] EQ 0 THEN return,0 ELSE BEGIN
      k = array
      IF n_elements(k) EQ 1 THEN return,total(dims,1)
    ENDELSE 
  ENDIF
  IF n_elements(dims[*,0]) NE n_elements(k) THEN return,0 ELSE BEGIN 
    klocs =  transpose([1l,long(product(reverse(((reverse(reform(k)))[1:*])),/cum))])
    dlocs = long(dims) ## klocs
    return,reform(dlocs)
  ENDELSE 
END
