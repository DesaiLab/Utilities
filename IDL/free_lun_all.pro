PRO Free_lun_all,more=more
  IF keyword_set(more) THEN start = 2 ELSE start = 101
  FOR i = start,128 DO free_lun,i
END
