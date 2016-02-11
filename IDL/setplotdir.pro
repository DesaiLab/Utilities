PRO SetPlotDir,dr,mydoc=mydoc
  IF keyword_set(mydoc) THEN outdr = Mydocs()+dr ELSE outdr = dr
  defsysv,'!PLOTDIR',outdr
END
