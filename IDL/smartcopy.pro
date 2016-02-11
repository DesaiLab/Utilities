PRO smartcopy,t
  str = strsplit(t,string(13b),/extract)
  glines = where(strupcase(str_left(str,4)) EQ 'IDL>',ng)
  IF ng GT 0 THEN BEGIN
    str = strmid(str[glines],5)
    outline = ''
    FOR i = 0,ng-1 DO outline+=str[i]+string(13b)+string(10b)
  ENDIF 
  a = obj_new('IDLcomIDispatch$PROGID$JSSys3.Ops')
  a->SendTextCB,outline
  obj_destroy,a
;stop
END
