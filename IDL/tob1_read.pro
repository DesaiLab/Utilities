;**HELPER FUNCTIONS**
FUNCTION tob1_binbyte,bits
;convert bits into a number
  return,long(total(reverse(bits)*(2l^lindgen(n_elements(bits))),/integer))
END 

FUNCTION tob1_bytebin,byt
;convert number into binary, pad to make at least 8 bits
  num = strtrim(string(byt,format='(b-32.8)'),2)
  return, byte(fix(strmid(num,indgen(strlen(num)),replicate(1,strlen(num)))))
END

;**MAIN FUNCTION**
FUNCTION tob1_read,fname,header=header
;Function Campbell Scientific TOB1 file and return array of data

;USAGE
;data = tob1_read(fname,header=header)
;fname = Filename
;data = output as floating point array
;header = header info

  IF n_elements(fname) EQ 0 THEN BEGIN
    print,'No filename specificied'
    return,!values.f_nan
  ENDIF 

  IF ~file_test(fname,/read) THEN BEGIN
    print,'Filename '+fname+' can not be opened for reading'
    return,!values.f_nan
  ENDIF 

;Get total size in bytes of file and open file
  file_size = (file_info(fname)).size
  openr,fl,fname,/get_lun

;First five lines are string headers (separated by newline), read them
;Here is an example of such:
;"TOB1","LimnologyFlux","CR1000","23193","CR1000.Std.24","CPU:OPEC_PSU_cherrey.cr1","53300","ts_data"
;"SECONDS","NANOSECONDS","RECORD","Ux","Uy","Uz","Ts","co2","h2o","diag"
;"SECONDS","NANOSECONDS","RN","m/s","m/s","m/s","C","mg/m^3","g/m^3","unitless"
;"","","","Smp","Smp","Smp","Smp","Smp","Smp","Smp"
;"ULONG","ULONG","ULONG","IEEE4","IEEE4","IEEE4","IEEE4","IEEE4","IEEE4","FP2"

  header = strarr(5)
  readf,fl,header  ;Read 1st five lines into array

;Check for valid filetype
  filetype = strmid(header[0],0,6)
  IF filetype NE '"TOB1"' THEN BEGIN
    print,'File is not TOB1, return value: '+filetype
    free_lun,fl
    return,!values.f_nan
  ENDIF 

;Get vartypes (ULONG, IEEE4 or FP2) in an array
  varnames = strsplit(header[1],',',/extract)
  vartypes = strsplit(header[4],',',/extract)
  n_vars = n_elements(varnames)

  IF n_vars LT 3 THEN BEGIN
    print,'Not enough columns (< 3) in header '+varnames
    free_lun,fl
    return,!values.f_nan
  ENDIF 

;First three should always be ULONG Time stamp
  IF (varnames[0] NE '"SECONDS"') OR (varnames[1] NE '"NANOSECONDS"') OR (varnames[2] NE '"RECORD"') $
     OR (vartypes[0] NE '"ULONG"') OR (vartypes[1] NE '"ULONG"') OR (vartypes[2] NE '"ULONG"') THEN BEGIN
    print,'Error in file format, expecting Seconds,Nanoseconds,Record of type ULONG, but have '+varnames[0:2]
    free_lun,fl
    return,!values.f_nan
  ENDIF 

;Calculate number of records
;subtract header from total filesize, then 
;4 bytes per IEEE4 or ULONG, 2 bytes per FP2
  record_size = 0l
  FOR i = 0,n_vars-1 DO BEGIN
    CASE vartypes[i] OF 
      '"ULONG"' : record_size += 4
      '"USHORT"' : record_size += 2
      '"IEEE4"' : record_size += 4
      '"FP2"' :  record_size += 2
      ELSE : BEGIN 
        print,'Unknown var type '+varnames
        free_lun,fl
        return,!values.f_nan
      ENDELSE 
    ENDCASE 
  ENDFOR 

  current_fileloc = (fstat(fl)).cur_ptr
  n_records = (file_size-current_fileloc)/record_size
  n_data = n_vars-3
  n_columns = n_data + 7 ;Year, Month, Day, Hour, Minute, Second, Record

;Output double precision array
  outdata = make_array(n_columns,n_records,/double,value=!values.f_nan)
  
;Time stamp is stored as seconds since midnight, Jan 1, 1990 
;Can use IDL julday/caldat functions to do math for us
;Time stamp has 3 elements: seconds+nanoseconds+record 
  jd1990 = julday(1,1,1990,0,0,0)  
  time_stamp = ulonarr(3)

  print,'Ready to read file '+fname+' columns: '+string(n_vars)+' lines: '+string(n_records)

;Read one line at a time
  curlin = 0l
  WHILE ~eof(fl) DO BEGIN
    IF curlin MOD 10000 EQ 0 THEN print,'  Reading line '+string(curlin)
;Read time stamp and convert to a useful time
    readu,fl,time_stamp
    jd = jd1990 + ((double(time_stamp[0])+double(time_stamp[1]/1e9))/86400d)
    caldat,jd,mon,day,yr,hr,min,sec
    outdata[0:6,curlin] = [yr,mon,day,hr,min,sec,time_stamp[2]] ;implict type conversion from ulong to double
;Read each data type
    FOR i = 0,n_data-1 DO BEGIN 
      CASE vartypes[i+3] OF
        '"ULONG"' : BEGIN 
          data = ulong(0)
          readu,fl,data
        END
        '"USHORT"' : BEGIN
          data = uint(0)
          readu,fl,data
        END 
        '"IEEE4"' : BEGIN 
          data = 0.             ;floating point
          readu,fl,data
        END
        '"FP2"' : BEGIN 
          byt1 = 0b             ;Campbell 2 byte float
          byt2 = 0b
          readu,fl,byt1
          readu,fl,byt2
          bit1 = tob1_bytebin(byt1)
          bit2 = tob1_bytebin(byt2)
          sign = (bit1[0]*(-2.0))+1.0
          expo = tob1_binbyte([bit1[1],bit1[2]])
          num = tob1_binbyte([bit1[3:7],bit2])
          data = sign * float(num) * (10^((-1.0)*expo))
         END
      ENDCASE 
      outdata[i+7,curlin] = data
    ENDFOR 
    curlin++
  ENDWHILE 
  free_lun,fl

  return,outdata
END


