#lang basic
10 if 2 < 4 then print "true" else print "false"
20 if 2 > 4 then print "true" else print "false"
30 if 2 > 4 then goto 50
40 print "not"
50 print "true"
60 if 2 < 4 then 40 + 40 else 70
70 print "not"
80 print "true"
