PRO map_provinces
;  MAP_SET, /robinson,0, 0,/GRID, COLOR=black,limit=[30.0,-150.0,70.0,-30.0]
;  map_continents,/coasts,/countries
  myshape = obj_new('IDLffShape',mydocs()+'idl/util/province.shp')
  myshape -> IDLffShape::GetProperty, N_ENTITIES=num_ent
  FOR x = 0,num_ent-1 DO BEGIN
    attr = myshape -> IDLffShape::GetAttributes(x)
;    print,'Drawing ',attr.attribute_1
    ent = myshape -> IDLffShape::GetEntity(x)
    parts = *(ent.parts)
    vertices = *(ent.vertices)
    n_vertices = ent.n_vertices
    n_parts = ent.n_parts
    IF n_parts EQ 0 THEN BEGIN
;      print,'  No parts in this province'
      oplot,vertices[0,*],vertices[1,*]
    ENDIF ELSE BEGIN 
      FOR y = 0,n_parts-1 DO BEGIN
        first = parts[y]
        IF y NE n_parts-1 THEN last = parts[y+1]-1 ELSE last = n_vertices-1
        oplot,vertices[0,first:last],vertices[1,first:last]
      ENDFOR
    ENDELSE
    myshape -> IDLffShape::DestroyEntity, ent
  ENDFOR
  myshape -> close
  obj_destroy,myshape
END
