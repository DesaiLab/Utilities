FUNCTION t_code,typename
  CASE strupcase(strtrim(typecode,2)) OF
    'UNDEFINED' : type = 0
    'BYTE' : type = 1
    'INT' : type = 2
    'LONG' : type = 3
    'FLOAT' : type = 4
    'DOUBLE' : type = 5
    'COMPLEX' : type = 6
    'STRING' : type = 7
    'STRUCT' : type = 8
    'DCOMPLEX' : type = 9
    'POINTER' : type = 10
    'OBJECT' : type = 11
    'UINT' : type = 12
    'ULONG' : type = 13
    'LONG64' : type = 14
    'ULONG64'  : type = 15
  ENDCASE
  return,type
END
