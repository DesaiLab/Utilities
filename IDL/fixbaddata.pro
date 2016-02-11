PRO fixbaddata,fname
  basedirectory = ''
  inf = basedirectory+fname+'.gz'
  print,'Opening '+inf
  openr,f,inf,/get_lun,/compress
  IF file_test(inf,/read) THEN BEGIN 
    cnt = 0l
    outf = basedirectory+fname+'.fix.gz'
    openw,f2,outf,/get_lun,/compress
    WHILE NOT eof(f) DO BEGIN
      cnt = cnt+1
      IF cnt MOD 100000l EQ 0 THEN print,'On line ',CNT
      Q = 0B
      READU,F,Q
      IF Q EQ 30B THEN Q = 10B
      writeu,f2,q
    ENDWHILE
    free_lun,f
    free_lun,f2
    print,'Wrote '+outf
  ENDIF ELSE print,'Could not open file for input: '+inf
END
