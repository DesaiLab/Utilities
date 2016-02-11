PRO netcdf_test
;example netcdf file generator

;typical dimensions
; time = UNLIMITED ; //(xxx currently )
; y = XXX;
; x = XXX;

;variables (with attributes):
; float uwnd(time,y,x)  ;use NEON names
; uwnd:units = "m/s"
; uwnd:long_name = "descript"
; uwnd:valid_range = -xxx,xxx ;
; uwnd:_FillValue = NAN;
; uwnd:missing_value = NAN;
; uwnd:add_offset = 0.f;  valid range should be that off packed data
; uwnd:scale_factor = 1.0f;


; double time(time)
;  time:units = "hours since YYYY-M-D HH:MM:S.S UTC"  ;or seconds
;  time:time_origin = " YYYY-MON-DD HH:MM:SS"
;  time:long_name = "Time" ;
;  time:axis = "T" ;
;  time:standard_name = "time"
;  time:coordinate_defines = "point"
;  time:delta_t = "YYYY-MM-DD HH:MM:SS.SS" ;  0000-00-00 00:00:00.1
;  time_actual_range = X,X;

;global attributes:
; lat =
; lon =
; title =
; sonic_axis = 189.28
; sonic_type = 'ATI Type K'
; sonic_signconvention = "+u = into sonic, +w = upward, +v = left to right (righthand rule)"
; measurement_height = '30 m'
; IRGA_type = ''
; processing_applied = "calibration, rotation, lags"
; tube_length = 
; tube_diameter =
; inlet_gap = '1 m'


  id = ncdf_create('/Users/adesai/fname.nc',/clobber)
  ncdf_control,id,/fill
  time = findgen(864000)/10.0
  
  uwnd = fltarr(864000)
  uwnd[36000:36100]=nan()

  tid = ncdf_dimdef(id,'time',/unlimited)
  
  time_id = ncdf_vardef(id,'time',[tid],/double)
  uwnd_id = ncdf_vardef(id,'uwnd',[tid],/float)
  
  ncdf_attput,id,time_id,'units','seconds since 2011-07-13 00:00:00.0 UTC'
  ncdf_attput,id,time_id,'time_origin','2011-JUL-13 00:00:00'
  ncdf_attput,id,time_id,'long_name','Time'
  ncdf_attput,id,time_id,'axis','T'
  ncdf_attput,id,time_id,'delta_t','0000-00-00 00:00:00.1'
  ncdf_attput,id,time_id,'actual_range',[36000.0,71999.0]

  ncdf_attput,id,uwnd_id,'units','m/s'
  ncdf_attput,id,uwnd_id,'long_name','wind speed into sonic'
  ncdf_attput,id,uwnd_id,'valid_range',[-40.0,40.0]
  ncdf_attput,id,uwnd_id,'_FillValue',nan()
  ncdf_attput,id,uwnd_id,'missing_value',nan()
  ncdf_attput,id,uwnd_id,'add_offset',0.0
  ncdf_attput,id,uwnd_id,'scale_factor',1.0

  ncdf_attput,id,/global,'title','High frequency observations from US-PFa WLEF tall tower'
  ncdf_attput,id,/global,'sonic_axis_from_true_north',189.28
  ncdf_attput,id,/global,'latitude',45.946
  ncdf_attput,id,/global,'longitude',-90.272

  NCDF_CONTROL, id, /ENDEF

  ncdf_varput,id,time_id,time[36000:71999]
  ncdf_varput,id,uwnd_id,uwnd[36000:71999]
  ncdf_close,id

  stop





END
