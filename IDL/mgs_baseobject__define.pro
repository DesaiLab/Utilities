;+
; NAME:
;   mgs_baseobject (object)
;
; PURPOSE:
;      This object provides basic functionality in terms of error
;   handling and allows the user to store an object name as well as
;   other arbitrary information in the UValue field. The user can
;   decide if error messages shall be displayed as a dialog or in the
;   log window, and a debug flag can be set which will cause traceback
;   information to be displayed if an error occurs. The base object
;   also provides a mechanism to copy any derived object with all its
;   data (cloning).
;      The base object provides the following methods, most of which
;   will likely have to be extended for derived objects:
;     (f=function, p=procedure)
;     Init(f)        : initialize object values
;     Cleanup(p)     : destroy all object data and remove object
;     SetProperty(p) : change object properties
;     GetProperty(p) : retrieve object properties
;     Undefine(p)    : render a variable argument as undefined
;     ErrorMessage(p): display an error message as dialog or on the log
;                      screen, traceback information if object in
;                      debug mode.
;     Copy(f)        : create a clone of the object and all its contents
;
; AUTHOR:
;
;   Dr. Martin Schultz
;   Max-Planck-Institut fuer Meteorologie
;   Bundesstr. 55, D-20146 Hamburg
;   email: martin.schultz@dkrz.de
;
; CATEGORY:
;   General object programming
;
; CALLING SEQUENCE:
;     This object will generally not be instanced in itself. To inherit
;     from it, add  { ..., INHERITS MGS_BaseObject } to your
;     object__define procedure, call self->MGS_BaseObject::Init at the
;     beginning of your new object's Init method, and
;     self->MGS_BaseObject::Cleanup at the end of your new object's
;         Cleanup method.
;     The MGS_BaseObject::SetProperty and GetProperty methods should
;     always be called at the beginning of a derived object's
;     SetProperty or GetProperty method to ensure that these values
;     are up-to-date in case they are needed.
;
; EXAMPLE:
;     See any of the objects in the MGS_ class hierarchy.
;
; MODIFICATION HISTORY:
;   Martin G. Schultz, 25 May 2000: VERSION 1.00
;   mgs, 07 Jun 2000 : - now allows for multiple line error messages
;   mgs, 09 Jun 2000 : - added Copy and CopyPointers methods
;   mgs, 04 Mar 2001 : VERSION 1.10
;       - improved error message, added debug flag, cleaned code
;       - Validate now a function instead of a procedure and actually
;         used
;   mgs, 10 Mar 2001: - now multiple debug levels allowed
;   mgs, 25 Apr 2001: - removed Validate (now in BaseGUI)
;                     - bug fix in ErrorMessage (wrong caller determined)
;   mgs, 10 May 2001: - added GetState method
;   mgs, 15 May 2001: - bug fix in ErrorMessage (theMessage arg not
;                       present); thanks Ben
;
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000-2001 Martin Schultz
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################




; =====================================================================================
; Base object methods:
;     ErrorMessage: display an error message as dialog or screen message
;     Undefine    : render an argument variable undefined
;     GetState    : return object data as structure. ONLY FOR DEBUGGING!!

; -------------------------------------------------------------------------------------
; Message / ErrorMessage:
;    This procedure displays an error (or warning or info) message
; with optional traceback information. Default is to display a
; Dialog_Message, unless the object's no_dialog flag is set or the
; no_dialog keyword is provided at the call of this method. IDL
; versions prior to 5.3 need also ensure that the current device
; supports widgets.  This method is particularily useful in Catch
; error handlers. See other methods of this object for examples.
;
; As it is not possible to call a method from an uninitialized object,
; this routine is written as a standard procedure instead of an object
; method. Once, the baseobject has been successfully initialized, you
; can call the Errormessage method instead of the procedure which
; makes your programs look more consistent. In practice, you will
; always be able to use the ErrorMessage method because the first
; thing to do when you inherit from baseobject is to call the
; baseobject's Init method, and - voila - the ErrorMessage method is
; defined.
;
; Authors: David Fanning wrote the original Error_Message program and
; introduced the traceback functionality. Martin Schultz adapted his
; program for use with objects and modified it to accept string arrays
; as message. (mgs, 04 MAR 2001)

pro MGS_BaseObject_Message,  $
           thisMessage,          $ ; The text of the error message
           no_dialog=no_dialog,  $ ; Don't display a dialog
           No_ErrorState=No_ErrorState,  $ ; Don't display the error status
           Warning=Warning,  $     ; Display a warning instead of an error
           Information=info, $     ; Display an Info message instead of an error
           Traceback=traceback, $  ; Display trace information
           _Extra=extra         ; Use extra keywords to control behaviour
                                ; of internal errors (passed on to Message) or to
                                ; extend the ErrorMessage method's functionality.

   On_Error, 2       ;; return to caller if internal error occurs

   ;; establish own error handler
   ;; internalMessage = 'Strange internal error.'
   CATCH, theError
   IF theError NE 0 THEN BEGIN
       CATCH, /Cancel
       Message, internalMessage, _Extra=extra
       RETURN       ;; return needed in case user supplies /Continue keyword
   ENDIF

   ;; --- 1. Check keywords and collect all information
   internalMessage = 'Internal error in MGS_BaseObject_ErrorMessage.'
   systemMessage = !Error_State.Msg

   ;; Determine type of message (warning overrides info overrides error)
   type = 0     ;; default = error message
   IF Keyword_Set(info) THEN type = 2
   IF Keyword_Set(warning) THEN type = 1
   typename = [ 'Error', 'Warning', 'Info' ]

   ;; Check for presence of message and make sure it's a string.
   ;; Make local copy since string is manipulated
   ;; Compose message string
   IF N_Elements(thisMessage) EQ 0 THEN BEGIN
      IF type EQ 0 THEN BEGIN
         theMessage = systemMessage  ;; even if No_ErrorState is set!
      ENDIF ELSE BEGIN
         RETURN          ;; empty warning or info makes no sense
      ENDELSE
   ENDIF ELSE BEGIN
      theMessage = StrTrim(thisMessage,2)
      IF theMessage[0] EQ '' THEN theMessage[0] = 'Unspecified '+typename[type]+'.'
      ;; Append system error message if necessary
      IF type EQ 0 AND Keyword_Set(No_ErrorState) EQ 0 THEN $
        theMessage = [ theMessage, systemMessage ]
   ENDELSE

   ;; Get calling routine's name
   Help, Calls=CallStack
   IF N_Elements(CallStack) GE 3 THEN BEGIN
      Caller = ( StrSplit( StrCompress(CallStack[2]), " ", /Extract ) )[0]
      Caller = StrUpCase(Caller)
      IF Caller EQ '$MAIN$' THEN Caller = '' ELSE Caller = Caller + ': '
   ENDIF ELSE BEGIN
      Caller = ''
   ENDELSE


   ;; Get traceback information
   IF Keyword_Set(traceback) THEN Help, /Last_Message, Output=tracebackinfo

   ;; --- 2. Display message
   ;; Determine whether dialog shall be used:
   ;; - default is object's no_dialog value
   ;; - this gets overwritten with local no_dialog keyword
   ;; - finally a test is made whether current device supports dialogs
   ;;   at all (only necessary for IDL < 5.3)

   Use_Dialog = 1 - Keyword_Set(no_dialog)
   IF FLOAT(!Version.Release) LT 5.3 AND (!D.FLAGS AND 65536L) EQ 0 THEN $
     Use_Dialog = 0    ; cannot display dialog on current device

   ;; Display error message as dialog
   IF Use_Dialog THEN BEGIN
      ;; Prepend first line of message with name of calling routine
      theMessage[0] = Caller+theMessage[0]
      ok = Dialog_Message(theMessage, Error=(type EQ 0), Info=(type EQ 2), _Extra=extra)
   ENDIF ELSE BEGIN
      ;; Display error message as text
      Message,theMessage[0], /Info
      IF N_Elements(theMessage) GT 1 THEN BEGIN
         FOR i=1L, N_Elements(theMessage)-1 DO $
            Message,theMessage[i], /Info, /NoName
      ENDIF
   ENDELSE

   ;; Display traceback information
   IF type EQ 0 AND Keyword_Set(traceback) THEN BEGIN
      Print,''
      Print, 'Traceback Report from ' + StrUpCase(Caller) + ':'
      Print, ''
      IF tracebackInfo[0] NE '' Then BEGIN
      		;a general case error, display the traceback info
      	FOR j=0,N_Elements(tracebackinfo)-1 DO Print, "     " + tracebackinfo[j]
      ENDIF ELSE BEGIN
      		;an error where the the message is provided by the caller
      	Print, TheMessage
      	n = n_elements(CALLSTACK)
		If n_elements(CALLSTACK) GT 2 Then $
			For j = 2, n-1 do Print,    "     " +CALLSTACK[j] ELSE $ ;general error
			  For j = 0, n-1 do Print,    "     " + CALLSTACK[j]   ;error within error handler

		ENDELSE
   ENDIF

   ;; Clear error status
   Message, /Reset

end



PRO MGS_BaseObject::ErrorMessage,  $
                  theMessage,  $
                  No_Dialog=no_dialog, $
                  Traceback=traceback,  $
                  _Extra=extra

   ;; simply call the error message procedure defined above and use
   ;; object's no_dialog value unless overwritten
   IF N_Elements(no_dialog) EQ 0 THEN no_dialog = self.no_dialog
   IF N_Elements(traceback) EQ 0 THEN traceback = Keyword_Set(self.debug)


   ;; Add object name to error message if defined
   IF N_Elements(theMessage) GT 0 THEN BEGIN
     IF self.name NE '' THEN BEGIN
         displaymessage = [ self.name, theMessage ]
      ENDIF ELSE BEGIN
         displaymessage = theMessage
      ENDELSE

   ENDIF

   MGS_BaseObject_Message, $
     displayMessage, $
     no_dialog=no_dialog, $
     traceback=traceback, $
     _Extra=extra

END


; -------------------------------------------------------------------------------------
; Undefine:
; This method applies a trick published by Andrew Coole, Adelaide, AUS
; to render an undefined variable from within an IDL subroutine.

pro MGS_BaseObject::Undefine, arg

   if (n_elements(arg) eq 0) then return   ; already undefined
   tempvar = SIZE(TEMPORARY(arg))

end

; -------------------------------------------------------------------------------------
; CopyPointers:  (private)
;   This method returns a physical copy of the pointer or pointer
; array given as argument. If a pointer contains other pointers, the
; method is called recursively.
;   If the pointer or pointer array contains object references, an
; attempt is made to call that object's Copy method so that the object
; is copied physically rather than only by reference. If an object has
; no Copy method (or if you want to preserve the link), set the
; no_objects keyword.


FUNCTION MGS_BaseObject::CopyPointers, p, no_objects=no_objects

   ;; Set dummy default
   result = Ptr_New()

   ;; Find out if p is a pointer array
   N = N_Elements(p)
   IF N GT 1 THEN result = PtrArr(N)

   ;; Go through elements of p and duplicate them
   FOR i=0L, N-1 DO BEGIN
      IF Ptr_Valid( p[i] ) THEN BEGIN
         ;; Check if pointer contains another pointer
         type = Size( *p[i], /TName )
         IF type EQ 'POINTER' THEN $                 ; recursive pointer copy
            result[i] = Ptr_New( self->CopyPointers( *p[i] ) ) $
         ELSE IF type EQ 'OBJREF' AND not Keyword_Set(no_objects) THEN BEGIN
            result[i] = Ptr_New( (*p[i])->Copy() )   ; physical copy of object
         ENDIF ELSE  $
            result[i] = Ptr_New( *p[i] )             ; copy of object reference
      ENDIF
   ENDFOR

   RETURN, result
END


; -------------------------------------------------------------------------------------
; Copy:
; This method makes a copy of the object itself (cloning). The result
; is an empty (invalid) object reference if something goes
; wrong. Pointers are copied explicitely unless the no_pointers
; keyword is set. Objects are copied via recursive calls to their Copy
; method (which means they should have one). If you don't want to copy
; the objects but only the object references, use the no_objects
; keyword. With the free_pointers and free_objects keywords you can
; instead delete the pointer and object references from the clone.

FUNCTION MGS_BaseObject::Copy, no_pointers=no_pointers, $
                       no_objects=no_objects,  $
                       free_pointers=free_pointers,  $
                       free_objects=free_objects,  $
                       verbose=verbose

   ;; set default result
   dummy = Obj_New()

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      self->ErrorMessage, 'Error building object clone!'
      IF Obj_Valid(clone) THEN Obj_Destroy, clone
      RETURN, dummy
   ENDIF

   ;; If object in debug mode, always be verbose
   IF Keyword_Set(self.debug) THEN verbose = 1

   ;; Initialize an empty object of the same type as self
   clone = Obj_New( Obj_Class(self) )

   ;; Do a structure assignment to copy all fields verbatim
   Struct_Assign, self, clone, verbose=Keyword_Set(verbose)

   ;; Now apply a trick to get a structure with the current
   ;; object's tag names and types
   ;; (courtesy James Tappin)
   ok = Execute('ostru = {'+Obj_Class(self)+'}')
   IF not ok THEN RETURN, dummy

   ;; Go through tags and check which ones are pointers or objects:
   ;; these must be copied explicitely
   IF Keyword_Set(verbose) THEN help,ostru,/stru

   ;; Pointers
   FOR i=0L, N_Tags(ostru)-1 DO BEGIN
      tname = Size(ostru.(i), /TName)
      ;; Copy pointer contents or free them
      IF tname EQ 'POINTER' THEN BEGIN
         IF Keyword_Set(free_pointers) THEN BEGIN
            clone.(i) = Ptr_New()
         ENDIF ELSE IF not Keyword_Set(no_pointers) THEN BEGIN
            IF Keyword_Set(verbose) THEN  $
               print,'Copying pointer ',(tag_names(ostru))[i],'...'
            clone.(i) = self->CopyPointers( self.(i), no_objects=no_objects )
         ENDIF
      ENDIF
      ;;
   ENDFOR

   ;; Objects
   FOR i=0L, N_Tags(ostru)-1 DO BEGIN
      tname = Size(ostru.(i), /TName)
      ;; Copy pointer contents or free them
      IF tname EQ 'OBJREF' THEN BEGIN
         IF Keyword_Set(free_objects) THEN BEGIN
            clone.(i) = Obj_New()
         ENDIF ELSE IF not Keyword_Set(no_objects) THEN BEGIN
            IF Keyword_Set(verbose) THEN  $
               print,'Copying object ',(tag_names(ostru))[i],'...'
            IF Obj_Valid( clone.(i) ) THEN $
               clone.(i) = self.(i)->Copy()
         ENDIF
      ENDIF
      ;;
   ENDFOR

   IF Keyword_Set(verbose) THEN print,'Done.'

   RETURN, clone

END


; -------------------------------------------------------------------------------------
; GetState:
; This method returns the object "data" as a structure. Pointers and
; object references are not copied, i.e. they are returned as
; unitialized variables (NullPointer)!

FUNCTION MGS_BaseObject::GetState

   ok = Execute('retval = {'+Obj_Class(self)+'}')  ;; Same trick as in Copy
   IF ok THEN BEGIN
      Struct_Assign, self, retval
   ENDIF ELSE BEGIN
      retval = { nothing: 0L }
   ENDELSE

   RETURN, retval

END


; =====================================================================================
; Standard object methods:
;     GetProperty : retrieve object values
;     SetProperty : set object values
;     Cleanup     : free object pointers and destroy
;     Init        : initialize object

; -------------------------------------------------------------------------------------
; GetProperty:
; This method extracts specific object values and returns them to the
; user. Derived objects should overwrite and extend this method to
; return the extra information stored in them. Note that a distinction
; should be made between simple requests which assign object values to
; local variables and requests that may take more time because a lot
; of data may be involved or a complicate function may be called. The
; latter should always make use of the Arg_Present function.

PRO MGS_BaseObject::GetProperty,  $
                   name=name,        $ ; The variable name
                   uvalue=uvalue,    $ ; A user defined value
                   no_copy=no_copy,  $ ; Copy pointer values?
                   no_dialog=no_dialog, $  ; Object uses dialogs for messages?
                   debug=debug,      $ ; Object is in debug mode?
                   _Ref_Extra=extra


   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      self->ErrorMessage, 'Error retrieving baseobject properties!'
      RETURN
   ENDIF

   ;; Handle simple requests
   name = self.name
   no_copy=self.no_copy
   no_dialog = self.no_dialog
   debug = self.debug

   ;; Handle requests that may require more time
   IF Arg_Present(uvalue) THEN BEGIN
       IF Ptr_Valid(self.uvalue) THEN $
          uvalue = *self.uvalue  $
       ELSE  $
          self->undefine, uvalue
   ENDIF

END


; -------------------------------------------------------------------------------------
; SetProperty:
; This method sets specific object values. Derived objects should
; extend this method to allow storing additional information. The
; baseobject's SetProperty method should always be called at the
; beginning of the derived object's SetProperty method to ensure that
; the baseobject properties are up-to-date.
; Note: If result is used in derived objects it must be an explicit
; keyword of the derived SetProperty method.

PRO MGS_BaseObject::SetProperty,  $
                   name=name,  $           ; The variable name
                   uvalue=uvalue,        $ ; A user defined value
                   no_copy=no_copy,      $ ; No_copy flag for ptr_new (used with uvalue)
                   no_dialog=no_dialog,  $ ; Flag to indicate if interactive dialogs are used
                   debug=debug,          $ ; Debugging level
                   _Extra=extra


   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      self->ErrorMessage, 'Error setting baseobject properties!'
      RETURN
   ENDIF

   ;; Test arguments
   IF N_Elements(name) GT 0 THEN self.name = name
   IF N_Elements(no_copy) GT 0 THEN self.no_copy = Keyword_Set(no_copy)
   IF N_Elements(no_dialog) GT 0 THEN self.no_dialog = Keyword_Set(no_dialog)
   IF N_Elements(debug) GT 0 THEN self.debug = fix(debug[0])

   IF N_Elements(uvalue) GT 0 THEN BEGIN
       IF Ptr_Valid(self.uvalue) THEN Ptr_Free, self.uvalue
       self.uvalue = Ptr_New(uvalue, no_copy=self.no_copy)
   ENDIF

END


; -------------------------------------------------------------------------------------
; Cleanup:
; This method frees all data stored in the object. Derived objects
; should call self->MGS_BaseObject::Cleanup at the end of their own Cleanup
; method.

PRO MGS_BaseObject::Cleanup

   IF Ptr_Valid(self.uvalue) THEN Ptr_Free, self.uvalue

END

; -------------------------------------------------------------------------------------
; Init:
; This method initializes the object values. The object can be named
; and the user can supply a UValue of any kind. The logical flags
; No_Dialog and Debug determine the appearance of the object's (error)
; messages. No_Dialog=1 will prevent dialog boxes from being
; displayed, and Debug=1 reports traceback information along with each
; error message.
;
; Derived objects should call self->MGS_BaseObject::Init at the
; beginning of their own Init method.
; Example:
;    IF not self->MGS_BaseObject::Init(name=name,...) THEN RETURN, 0
; Note that derived objects should use the _EXTRA keyword mechanism to
; handle the keywords for the MGS_BaseObject transparently.

FUNCTION MGS_BaseObject::Init,  $
                   name=name,  $           ; The object name
                   uvalue=uvalue,        $ ; A user defined value
                   no_copy=no_copy,      $ ; No_Copy flag for uvalue (Ptr_New)
                   no_dialog=no_dialog,  $ ; Flag indicating use of interactive dialogs
                   debug=debug,          $ ; Debugging level
                   _Extra=extra        ; For future additions

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      MGS_BaseObject_Message, 'Error initializing object: ', $
        no_dialog=Keyword_Set(no_dialog)
      RETURN, 0
   ENDIF

   ;; Test arguments and populate object
   IF N_Elements(name) EQ 0 THEN BEGIN
      self.name = ''
   ENDIF ELSE BEGIN
      self.name = StrTrim(name, 2)
   ENDELSE

   self.no_dialog = Keyword_Set(no_dialog)
   self.no_copy = Keyword_Set(no_copy)

   IF N_Elements(uvalue) GT 0 THEN BEGIN
      self.uvalue = Ptr_New(uvalue, no_copy=self.no_copy)
   ENDIF ELSE BEGIN
      self.uvalue = Ptr_New()
   ENDELSE

   IF N_Elements(debug) GT 0 THEN BEGIN
      self.debug = fix(debug[0])
   ENDIF ELSE BEGIN
      self.debug = 0
   ENDELSE

   ;; reset error state variable
   Message, /Reset

   RETURN, 1
END

; -------------------------------------------------------------------------------------
; This is the object definition. Derived objects should create a new
; structure and append these fields via the INHERITS MGS_BaseObject
; syntax.

PRO MGS_BaseObject__Define

   objectClass = { mgs_baseobject,       $ ; The object class
                   name          : '',   $ ; The object name
                   no_dialog     : 0,    $ ; Flag indicating use of interactive dialogs
                   debug         : 0,    $ ; Flag indicating debug mode
                   uvalue        : ptr_new(),  $  ; A user defined value
                   no_copy       : 0     $ ; Copy Pointer values?
                 }

END
