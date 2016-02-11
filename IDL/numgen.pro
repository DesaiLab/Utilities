FUNCTION numgen,a,b,int,type=type,numvals=numvals

;generates an array of numbers that ranges from a to b with interval int
;default interval is 1 and default type is type of <a> though can be set with type
;numvals overrides int and makes a through b with number of values

  on_error,2

  IF NOT keyword_set(b) THEN BEGIN
    b = a
    a = a - a
  ENDIF

  IF (a EQ b) AND keyword_set(numvals) THEN return,replicate(a,numvals)

  IF NOT keyword_set(type) THEN begin
    help,a,output=atype
    atype = (strtrim(strmid(atype,16,10),2))[0]
  ENDIF ELSE atype = (strupcase(type))[0]

  IF atype eq 'INT' THEN atypefunc = 'FIX' ELSE atypefunc = atype

  atypefunc2 = atypefunc
  IF (atypefunc2 EQ 'FIX') OR (strpos(atypefunc2,'LONG') GE 0) THEN atypefunc2 = 'ROUND'

  IF atype EQ 'STRING' THEN BEGIN
    IF strpos(a,'.') EQ -1 THEN atypefunc = 'LONG' ELSE atypefunc = 'DOUBLE'
  ENDIF

  oneval = call_FUNCTION(atypefunc,1)
  IF NOT keyword_seT(int) THEN intval = oneval ELSE intval = int
  IF (b LT a) AND (intval GT 0) THEN intval = intval * (-oneval)
  IF keyword_seT(numvals) THEN intval = (b-a) / (double(numvals)-1)
  aval = call_FUNCTION(atypefunc,a)
  bval = call_FUNCTION(atypefunc,b)

  IF intval EQ 0 THEN intval = oneval

  CASE atype OF
    'BYTE' : indfunc = 'bindgen'
    'COMPLEX' : indfunc = 'cindgen'
    'DCOMPLEX' : indfunc = 'dcindgen'
    'DOUBLE' : indfunc = 'dindgen'
    'FLOAT' : indfunc = 'findgen'
    'INT' : indfunc = 'indgen'
    'LONG' : indfunc = 'lindgen'
    'STRING' : indfunc = 'sindgen'
    'UINT' : indfunc = 'uindgen'
    'ULONG64' : indfunc = 'ul64indgen'
    'ULONG' : indfunc = 'ulindgen'
    ELSE : indfunc = 'findgen'
  endcase

  return, (call_FUNCTION(atypefunc2,(call_FUNCTION(indfunc,round(((bval-aval)/intval)+oneval)) * intval) + aval))

END
