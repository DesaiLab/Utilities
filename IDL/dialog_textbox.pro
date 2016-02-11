PRO dialog_textbox_event,event
  COMPILE_OPT hidden
  WIDGET_CONTROL, event.id, GET_UVALUE = whichevent
  IF N_ELEMENTS(whichevent) EQ 0 THEN RETURN
  IF whichevent NE "THEBUTTON" THEN RETURN

  CASE event.value OF
;OK
    0: BEGIN
      widget_control, event.top, get_uvalue = pvals
      (*pvals).okcancel = 1b
      widget_control, (*pvals).textbox, get_value = output
      (*pvals).output = output
      WIDGET_CONTROL, event.top, /DESTROY
    END 
;Cancel
    1: BEGIN
      WIDGET_CONTROL, event.top, get_UVALUE = pvals
      (*pvals).output = ''
      WIDGET_CONTROL, event.top, /DESTROY
    END 
    ELSE:
  ENDCASE 
END

PRO dialog_textbox_cleanup,id
  COMPILE_OPT hidden
  widget_control,id,get_uvalue=pvals
  IF (*pvals).okcancel eq 0b THEN (*pvals).output = ''
END

FUNCTION dialog_textbox,text=text,title=title,defaultoutput=defaultoutput,group_leader=group_leader, okcancel=okcancel
;create a modal widget to read user input
;choices are OK and Cancel

  IF n_elements(group_leader) EQ 0 THEN group_leader = WIDGET_BASE(MAP=0)

  Base = WIDGET_BASE(TITLE = title, /COLUMN, group_leader=group_leader,/modal,/align_center)
  choices =  Cw_Bgroup(Base, ['OK', 'Cancel'], /ROW, IDS=IDS, UVALUE="THEBUTTON")
  IF n_elements(text) EQ 0 THEN text = 'Input value:'
  IF n_elements(defaultoutput) EQ 0 THEN defaultoutput = ''
  input = cw_field(base,/string,title=text,/column,value=defaultoutput)
  okcancel = 0b
  vals = {okcancel:okcancel, output:defaultoutput, textbox:input}
  pvals = ptr_new(vals,/no_copy)
  widget_control,base,set_uvalue=pvals
  widget_control,base,/realize
  xmanager,"Dialog_TextBox",Base,cleanup='Dialog_Textbox_cleanup'

  okcancel = (*pvals).okcancel
  output = (*pvals).output
  ptr_free,pvals
  return,output
END
