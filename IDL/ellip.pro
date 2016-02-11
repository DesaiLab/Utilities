FUNCTION Ellip,RefEllip

;returns two values - equatorial radius and eccentricity squared
;Reference ellipsoids derived from Peter H. Dana's website
;http://www.utexas.edu/depts/grg/gcraft/notes/datum/elist.html
;Department of Geography, University of Texas at Austin
;Written by Chuck Gantz- chuck.gantz@globalstar.com
;Ported to IDL by Ankur Desai

Case strlowcase(strcompress(RefEllip,/remove_all)) of
  'airy' : Return,[6377563D, 0.00667054D]
  'australiannational' : Return,[6378160D, 0.006694542D]
  'bessel1841' : Return,[6377397D, 0.006674372D]
  'Bessel1841(nambia)' : Return,[6377484D, 0.006674372D]
  'clarke1866' : Return,[6378206D, 0.006768658D]
  'nad27' : return,[6378206D, 0.006768658D]
  'test' : return,[6378206D, 0.0067801506081D]
  'clarke1880' : Return,[6378249D, 0.006803511D]
  'everest' : Return,[6377276D, 0.006637847D]
  'fischer1960' : Return,[6378166D, 0.006693422D]
  'fischer1968' : Return,[6378150D, 0.006693422D]
  'grs1967' : Return,[6378160D, 0.006694605D]
  'grs1980' : Return,[6378137D, 0.00669438D]
  'helmert1906' : Return,[6378200D, 0.006693422D]
  'hough' : Return,[6378270D, 0.00672267D]
  'international' : Return,[6378388D, 0.00672267D]
  'krassovsky' : Return,[6378245D, 0.006693422D]
  'modifiedairy' : Return,[6377340D, 0.00667054D]
  'modifiedeverest' : Return,[6377304D, 0.006637847D]
  'modifiedfischer1960' : Return,[6378155D, 0.006693422D]
  'southamerican1969' : Return,[6378160D, 0.006694542D]
  'wgs60' : Return,[6378165D, 0.006693422D]
  'wgs66' : Return,[6378145D, 0.006694542D]
  'wgs72' : Return,[6378135D, 0.006694318D]
  'wgs84' : Return,[6378137D, 0.00669438D]
Else: Return,[6378137D, 0.00669438D]
EndCase

END

