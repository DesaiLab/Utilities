PRO RunTime,c
  a = systime(1)
  r = execute(c)
  print,systime(1)-a,' Seconds'
END
