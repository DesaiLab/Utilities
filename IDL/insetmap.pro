PRO InsetMap, bounds=bounds, limit=limit, grid=grid, box=box, b_limits=b_limits,fill=fill,isotropic=isotropic

  IF keyword_set(box) AND NOT keyword_set(b_limits) THEN map_proj_info,ll_limits=b_limits

  IF NOT keyword_set(bounds) THEN bounds = [0.6,0.2,0.9,0.5]
  IF NOT keyword_set(limit) THEN limit = [27.0,-122.0,49.0,-67.5]
  xsq = [bounds[0],bounds[2],bounds[2],bounds[0],bounds[0]]
  ysq = [bounds[1],bounds[1],bounds[3],bounds[3],bounds[1]]
  polyfill,xsq,ysq,/normal,color=!p.background
  plots,xsq,ysq,/normal
  map_set,mean([limit[0],limit[2]]),mean([limit[1],limit[3]]),position=bounds,limit=limit,/continents,/usa,/noerase,/albers,standard_parallels=[20.0,40.0],isotropic=isotropic,grid=grid,/noborder,clip=0

  IF keyword_set(box) THEN BEGIN
    bxsq = [b_limits[0],b_limits[2],b_limits[2],b_limits[0],b_limits[0]]
    bysq = [b_limits[1],b_limits[1],b_limits[3],b_limits[3],b_limits[1]]
    IF keyword_set(fill) THEN polyfill,bysq,bxsq,/data,color=fill
    plots,bysq,bxsq,/data
  ENDIF

END
