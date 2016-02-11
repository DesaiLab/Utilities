FUNCTION MyDocs
  defsysv,'!MYDOCS',exists = mde
  IF mde EQ 1 THEN base = !MYDOCS ELSE base = 'c:/Documents and Settings/adesai/My Documents/'
  return,base
;  return,'c:\Documents and Settings\psu\My Documents\adesai\'
END
