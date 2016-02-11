PRO Create_dir,dr,quiet=quiet
  rtch = str_right(dr,1)
  IF (rtch eq '\') OR (rtch EQ '/') THEN dir = str_strip(dr) ELSE dir = dr
  IF NOT file_test(dir,/directory) THEN BEGIN
    IF NOT keyword_set(quiet) THEN print,'Creating directory ',dir
    file_mkdir,dir
  ENDIF
END
