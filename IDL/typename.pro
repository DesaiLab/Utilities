function typename,typecode
  CASE typecode OF
    0 : type = 'UNDEFINED'
    1 : type = 'BYTE'
    2 : type = 'INT'
    3 : type = 'LONG'
    4 : type = 'FLOAT'
    5 : type = 'DOUBLE'
    6 : type = 'COMPLEX'
    7 : type = 'STRING'
    8 : type = 'STRUCT'
    9 : type = 'DCOMPLEX'
    10 : type = 'POINTER'
    11 : type = 'OBJECT'
    12 : type = 'UINT'
    13 : type = 'ULONG'
    14 : type = 'LONG64'
    15 : type = 'ULONG64'
  ENDCASE
  return,type
END
