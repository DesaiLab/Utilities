function RDFLD, dfname, fldname, dat, OFFSET=offset, COUNT=count

; example_1 (time series field, to be read in entirety):
;   nval_read = rdfld('/data/proja/sgpsondewrpnB1.a1.970618.142900.cdf', $
;                     'tdry', tdry)
;
; example_2 (2-d field, to be read in entirety):
;   nval_read = rdfld('sgpisobarsondes10p.c1.970717.000000.cdf', $
;                     'XY', xy_arr)
;
; example_3 (2-d field, to be read only starting at ix and iy):
;   nval_read = rdfld('sgpisobarsondes10p.c1.970717.000000.cdf', $
;                     'XY', xy_arr, offset=[ix-1,iy-1])
;   If desired, add count=[nx,ny], to avoid reading to the end of
;   the data field.  [Note: index x is the faster moving one,
;   but field description is displayed by ncdump utility,
;   as:  XY(y,x), in the C language convention, where x is
;   the faster moving index (just the opposite of Fortran)

; read a netCDF file, to
; return data for a variable (designated by fldname)

; if supplied, offset must be in the order a la Fortran:
; that is, OFFSET = [Ofs_var1, Ofs_var2, ...]
;         as in  dat(Sub_var1, Sub_var2, ...)
;   where the left most dimension moves the fastest;
; which is the opposite of ncdump listing,
;   where fldname = float( ..., Sub_var2, Sub_var1)
;   and the right most dimension moves the fastest
;
fid = NCDF_OPEN(dfname, /NOWRITE)
IF fid EQ -1  THEN BEGIN
  MESSAGE, 'NCDF_OPEN failed on: ' + dfname
ENDIF

vid = NCDF_VARID(fid, fldname)
IF vid EQ -1  THEN BEGIN
  MESSAGE, 'NCDF_VARID failed on: ' + fldname
ENDIF

IF KEYWORD_SET(offset)  THEN PRINT, 'OFFSET=',offset
vstruc = NCDF_VARINQ(fid, vid)

IF vstruc.NDIMS EQ 0  THEN BEGIN
  NCDF_VARGET, fid, vid, dat
ENDIF ELSE BEGIN
  IF NOT KEYWORD_SET(offset) THEN offset = INTARR(vstruc.NDIMS)
  IF NOT KEYWORD_SET(count)  THEN BEGIN
    NCDF_VARGET, fid, vid, dat, OFFSET=offset
  ENDIF ELSE BEGIN
    NCDF_VARGET, fid, vid, dat, OFFSET=offset, COUNT=count
  ENDELSE
ENDELSE

NCDF_CLOSE, fid

RETURN, N_ELEMENTS(dat)

END

Function RDFLD_HDF, dfname, fldname, dat, OFFSET = offset, COUNT = count, NUM = num

;read a HDF NetCDF-format SD file

sd_id = HDF_SD_START(dfname, /read)   	;opens an HDF file and initializes the SD interface
				       	;returns an SD interface ID

num = HDF_SD_NAMETOINDEX(sd_id, fldname)

;Fieldnames for AVHRR file
;avhrr_ch1, line, sample, avhrr_ch2, avhrr_ch3, avhrr_ch4, avhrr_ch5, rel_azimuth, sat_zenith, sun_zenith
;fieldnames for RAD file
;avhrr_ch3, line, sample, avhrr_ch4, avhrr_ch5
;Fieldnames for Lat Lon file
;latitude, line, sample, longitude


sds_id = HDF_SD_SELECT(sd_id,num)  ;Access the given SD in the HDF file - returns an SD dataset ID

If keyword_set(offset) Then Begin
  If keyword_set(count) Then Begin
    HDF_SD_GETDATA, sds_id, dat, START = offset, COUNT = count
  EndIf Else Begin
    HDF_SD_GETDATA, sds_id, dat, START = offset
  EndElse
EndIf Else Begin
  If keyword_set(count) Then Begin
    HDF_SD_GETDATA, sds_id,dat, COUNT = count
  EndIf Else Begin
    HDF_SD_GETDATA, sds_id, dat
  EndElse
EndElse

HDF_SD_ENDACCESS, sds_id             	;end access to any SD ids
HDF_SD_END, sd_id                     	;close the file

RETURN, N_ELEMENTS(dat)

END
