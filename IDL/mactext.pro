PRO mactext,fname
  fsize = (file_info(fname)).size
  dta = bytarr(fsize)
  openr,fl,fname,/get_lun
  readu,fl,dta
  free_lun,fl
  stop


  


END
