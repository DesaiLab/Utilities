;mainly to read NARR data 5 TB
;http://nomads.ncdc.noaa.gov:9091/dods/NCEP_NARR_DAILY/
;can write scripts to sequentially get lots of data

;can also use to get srad for ED

;returns a structure

;format:
;name of spawned command
;vars
;grid is a substructure

FUNCTION dods_typecode,str
  typenames = ['byte','int16','uint16','int32','uint32','float32','float64','str','url']
  typecodes = [1,2,12,3,13,4,5,7,7]
  loc = where(typenames EQ strtrim(strlowcase(str),2),nloc)
  IF nloc GT 0 THEN return,typecodes[loc[0]] ELSE return,-1
END

PRO dods_basetype,fl,strct,str,verbose=verbose
;basetype name (already read)
;data
;parse string, create val, add to end of strct
  starr = strtrim(strsplit(strlowcase(str),' ',/extract),2)
  tname = starr[0]
  tc = dods_typecode(tname)
  IF n_elements(starr) GT 1 THEN BEGIN
    vname = starr[1]
    IF tc EQ 7 THEN data = strjoin(replicate(' ',starr[2])) ELSE data = fix(0,type=tc)

    IF NOT eof(fl) THEN BEGIN 
      readu,fl,data
      IF keyword_set(verbose) THEN print,'Read single var ',vname
      IF vname NE 'error' THEN strct = create_struct(strct,vname,data) ELSE strct.error = 1b
    ENDIF ELSE BEGIN
      print,'Premature end of file!'
      strct.error = 1b
;      stop
    ENDELSE

  ENDIF ELSE BEGIN
    print,'No var name. error: ',starr
    strct.error = 1b
;    stop
  ENDELSE 

END


PRO dods_array,fl,strct,arrname=arrname,verbose=verbose
;Array (line already read)
;type name dimensions
;dimension sizes
;data
  str1 = ''
  str2 = ''
  IF NOT eof(fl) THEN BEGIN 
    readf,fl,str1
    str1 = strtrim(strsplit(strlowcase(Str1),' ',/extract),2)
    tname = str1[0]
    vname = str1[1]
    tcode = dods_typecode(tname)
    IF NOT eof(fl) THEN BEGIN 
      readf,fl,str2
      str2 = long(strtrim(strsplit(str2,' ',/extract),2))
      IF n_elements(str2) GT 0 THEN BEGIN 
        data = make_array(reverse(str2),type=tcode)
        IF NOT eof(fl) THEN BEGIN
          readu,fl,data
          IF keyword_set(verbose) THEN print,'Read array ',vname
          IF n_elements(arrname) NE 0 THEN vname = arrname
          strct = create_struct(strct,vname,data)
        ENDIF ELSE BEGIN
          print,'Premature end of file'
          strct.error = 1b
;          stop
        ENDELSE 
      ENDIF ELSE BEGIN
        print,'Array dimensions are bad ',str1,str2
        strct.error = 1b
;        stop
      ENDELSE 
    ENDIF ELSE BEGIN
      print,'Premature end of file'
      strct.error = 1b
;      stop
    ENDELSE 
  ENDIF ELSE BEGIN
    print,'Premature end of file'
    strct.error = 1b
;    stop
  ENDELSE 
END


PRO dods_grid,fl,strct,verbose=verbose
;create new struct, then append to big struct

;grid struct: name arrays maps

;Grid (Already read)
;gridname
;array narrays
;call dods_array for each array, add to struct
;if only one array, call it data
;maps nmaps
;call dods_array for each map array, add to struct


  IF NOT eof(fl) THEN BEGIN
    gname = ''
    readf,fl,gname
    IF keyword_set(verbose) THEN print,'Reading grid ',gname
    IF NOT eof(fl) THEN BEGIN
      gstrct = {name:gname}
      arrstr = ''
      readf,fl,arrstr
      arrstr = strsplit(strtrim(strlowcase(Arrstr),2),' ',/extract)
      IF arrstr[0] EQ 'array' THEN BEGIN
        narrays = long(arrstr[1])
        IF narrays EQ 1 THEN arrname = 'data'
        FOR i = 0,narrays-1 DO BEGIN
          str = ''
          IF NOT eof(fl) THEN BEGIN 
            readf,fl,str
            IF strtrim(strlowcase(str),2) EQ 'array' THEN BEGIN
              dods_array,fl,gstrct,arrname=arrname,verbose=verbose
            ENDIF ELSE BEGIN
              print,'Expecting Array, Got ',str
              strct.error = 1b
;              stop
            ENDELSE 
          ENDIF ELSE BEGIN
            print,'Premature end of file'
            strct.error = 1b
;            stop
          ENDELSE 
        ENDFOR

        IF (strct.error EQ 0b) AND (NOT eof(fl)) THEN BEGIN
          mstr = ''
          readf,fl,mstr
          IF mstr EQ '' THEN readf,fl,mstr
          mstr = strsplit(Strtrim(strlowcase(mstr),2),' ',/extract)
          IF mstr[0] EQ 'maps' THEN BEGIN
            nmaps = long(mstr[1])
            FOR i = 0,nmaps-1 DO BEGIN
              str = ''
              IF NOT eof(fl) THEN BEGIN 
                readf,fl,str
                IF str EQ '' THEN readf,fl,str
                IF strtrim(strlowcase(str),2) EQ 'array' THEN BEGIN
                  dods_array,fl,gstrct,verbose=verbose
                ENDIF ELSE BEGIN
                  print,'Expecting Array, Got ',str
                  strct.error = 1b
;                  stop
                ENDELSE 
              ENDIF ELSE BEGIN
                print,'Premature end of file'
                strct.error = 1b
;                stop
              ENDELSE 
            ENDFOR
            strct = create_struct(strct,gname,gstrct)
          ENDIF ELSE BEGIN
            print,'Expecting maps, got ',mstr
            strct.error = 1b
;            stop
          ENDELSE 
        ENDIF ELSE BEGIN
          print,'Premature end of file'
          strct.error = 1b
;          stop
        ENDELSE 
      ENDIF ELSE BEGIN
        print,'Expected array, got ',arrstr
        strct.error = 1b
;        stop
      ENDELSE 
    ENDIF ELSE BEGIN
      print,'Premature end of file'
      strct.error = 1b
;      stop
    ENDELSE 
  ENDIF ELSE BEGIN
    print,'Premature end of file'
    strct.error = 1b
;    stop
  ENDELSE 
;if grid, will have to look at more data to see more types

;grid structure
;Line1:  Grid
;call readgrid, send file handle and structure

;Line2:  gridname - will be struct tag
;(use rountine_names see xdf code to create vars)
;Line3:  array 1  (grid has one array) (read each array)

;array structure
;Line4: Array

;Line5: Float32 varname 3  
;   type name  numdimensions
;Line6: x y z
;   dimension info
;data

;maps structure
;maps 3

;Array
;Float64 time 1
;11
;data

;Array
;Float64 lat 1
;21
;data

;Array
;Float64 lon 1
;21
;data



END

FUNCTION loaddods,url,vars,ranges,verbose=verbose,opendap=opendap
;http://nomads.ncdc.noaa.gov:9091/dods/NCEP_NARR_DAILY/narr-b_221_rad.subset.dds

;need to spawn

;use create_struct(oldstruct,tag,value,tag,value)

;also fix and make_array
;typecodes in size

  app = '/data/apps/DODS/bin/writeval'
  IF ~keyword_set(opendap) THEN toturl = '"'+url ELSE toturl = url
  IF n_elements(vars) NE 0 THEN BEGIN
    toturl+='?'
    FOR i = 0,n_elements(vars)-1 DO BEGIN
      toturl+=vars[i]
      IF n_elements(ranges) NE 0 THEN IF n_elements(ranges) EQ 1 THEN toturl+=ranges ELSE toturl+=ranges[i]
      IF i NE n_elements(vars)-1 THEN toturl+=','
    ENDFOR 
  ENDIF 
  IF ~keyword_set(opendap) THEN toturl+='"'
  IF ~keyword_set(opendap) THEN BEGIN 
    cmd = app+' '+toturl
    spawn,cmd,unit=fl
    str = ''
    out = {cmd : cmd, error : 0b}
;not sure how to deal with strings or urls -
;cant deal with lists, sequences or structures yet
;just arrays and grids

    vectortypes = ['list','array']
    cmptypes = ['structure','sequence','grid']
  
    IF (fstat(fl)).read EQ 1 AND NOT eof(fl) THEN BEGIN 
      str = ''
      readf,fl,str
      str = strtrim(strlowcase(str),2)
      IF str EQ 'error' THEN BEGIN
        print,'Error reported: '
        WHILE NOT eof(fl) DO BEGIN
          readf,fl,str
          print,str
        ENDWHILE 
        free_lun,fl
;        stop
        out.error = 1b
        return,out
      ENDIF 
   
      
;parser
;big token can be one of typenames, array or grid
;if error, then break out
      REPEAT BEGIN 
;stop
        CASE str OF 
          'grid' : dods_grid,fl,out,verbose=verbose
          'array' : dods_array,fl,out,verbose=verbose
          'list' : BEGIN
            print,'Lists not supported'
            free_lun,fl
            out.error = 1b
            return,out
          END 
          'structure' : BEGIN
            print,'Structures not supported'
            free_lun,fl
            out.error = 1b
            return,out
          END 
          'sequence' : BEGIN
            print,'Sequences not supported'
            out.error = 1b
            free_lun,fl
            return,out
          END 
          '' : d=0
          ELSE : dods_basetype,fl,out,str,verbose=verbose
        ENDCASE 
        
        IF NOT eof(fl) THEN BEGIN 
          readf,fl,str
          str = strtrim(strlowcase(str),2)
        ENDIF
        
      ENDREP UNTIL eof(fl)
    ENDIF ELSE BEGIN
      print,'Unable to open stdout for reading data'
      out.error = 1b
      return,out
    ENDELSE 
    
    IF keyword_set(verbose) THEN print,'Success!'
    free_lun,fl
    return,out
  ENDIF ELSE BEGIN
    stat = opendap_get(toturl,out)
    return,out
  ENDELSE 

END
