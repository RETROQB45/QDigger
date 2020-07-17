DECLARE SUB CLEARMAZE ()
DECLARE SUB DEADSHAFT (x%, y%, n%)
DECLARE SUB DIGMAZE ()
DECLARE FUNCTION DIST! (a AS ANY, b AS ANY)
DECLARE SUB DRAWMAZE (x0%, y0%)
DECLARE SUB MAKELEV ()
DECLARE FUNCTION PATH% (x%, y%)
DECLARE SUB PLACECHARS ()
DECLARE SUB PLACEGOLD ()
DECLARE SUB PLACEJEWELS ()
DECLARE FUNCTION PTINRECT% (r%, x%, y%)
DECLARE FUNCTION RAND% (min%, max%)
DECLARE FUNCTION RECTSVALID% (BYVAL r1%, BYVAL r2%)
DECLARE SUB STORELEV (f%, idx$)
REM mklev v1.0 for arctic mine/qdigger

CONST FALSE = 0, TRUE = NOT FALSE

CONST COLORFUL = TRUE
CONST GOLDMIN = 4, GOLDMAX = 15
CONST JEWMIN = 25, JEWMAX = 100
DEFINT A-Z

TYPE XY
  x AS INTEGER
  y AS INTEGER
END TYPE
TYPE RECT
  x1  AS INTEGER
  y1  AS INTEGER
  x2  AS INTEGER
  y2  AS INTEGER
END TYPE

CONST SOIL = 0, EMPTY = 1, GOLD = 2, ENEM = 3, DIGGER = 4, JEWEL = 5
CONST MZW = 15, MZH = 10
DIM SHARED mz(0 TO MZW + 1, 0 TO MZH + 1) AS INTEGER
DIM SHARED rct(50) AS RECT
DIM SHARED BGR, nosel
DIM nmines AS LONG, i AS LONG
DIM SHARED d(3) AS XY: d(0).x = -1: d(1).y = -1: d(2).x = 1: d(3).y = 1

'DEBUG = TRUE
IF DEBUG THEN
  RANDOMIZE TIMER
  CLS
  MAKELEV
  DRAWMAZE 1, 1
  END
END IF

RANDOMIZE TIMER
PRINT "mklev, 2008"
PRINT "--"
INPUT "filename [mklev]"; file$
IF file$ = "" THEN file$ = "mklev"
IF INSTR(file$, ".") < LEN(file$) - 3 THEN file$ = file$ + ".mn"
INPUT "number of levels [8]"; nmines
IF nmines = 0 THEN nmines = 8
INPUT "background color [no]"; a$: BGR = (LCASE$(LEFT$(a$, 1)) = "y")
INPUT "select only good levels [yes]"; a$: nosel = (LCASE$(LEFT$(a$, 1)) = "n")
PRINT "--"
FOR i = 1 TO MZH + 1: PRINT : NEXT: y = CSRLIN - MZH - 1
f = FREEFILE
OPEN file$ FOR OUTPUT AS #f
t# = TIMER
FOR i = 1 TO nmines
  MAKELEV
  DRAWMAZE 1, y: COLOR 7: PRINT "maze"; i
  STORELEV f, LTRIM$(RTRIM$(STR$(i)))
NEXT
t# = TIMER - t#
PRINT #f, "_end"
CLOSE #f
PRINT "done: "; t#; "sec.": PRINT

INPUT "run game [yes,d=digger]"; a$
IF a$ = "" THEN a$ = "y"
a$ = LCASE$(LEFT$(a$, 1))
cmd$ = "arctic"
IF a$ = "d" THEN a$ = "y": cmd$ = "digger"
IF a$ = "y" THEN
  SHELL cmd$ + " " + file$
  PRINT
  PRINT "mklev: saved as "; file$
  PRINT
  WHILE INKEY$ = "": WEND
END IF
END

SUB CLEARMAZE
  FOR x = 1 TO MZW
    FOR y = 1 TO MZH
      mz(x, y) = SOIL
    NEXT
  NEXT
END SUB

SUB DEADSHAFT (x, y, n)
  IF n > 10 THEN EXIT SUB
  n = n + 1
  'CLS : DRAWMAZE 1, 1: SLEEP
  mz(x, y) = EMPTY
  c = 0
  done = FALSE
  DO
    dir = RAND(0, 3)
    x1 = x + d(dir).x
    y1 = y + d(dir).y
    IF x1 > 0 AND y1 > 0 AND x1 <= MZW AND y1 <= MZH THEN
      b = 0
      FOR i = -1 TO 1
        FOR j = -1 TO 1
          IF ABS(i) + ABS(j) <> 0 THEN
            x2 = x1 + i
            y2 = y1 + j
            IF mz(x2, y2) = EMPTY AND x2 <> x AND y2 <> y THEN b = b + 1
          END IF
        NEXT
      NEXT
      IF b = 0 THEN
        IF x1 = x AND y1 = y THEN STOP
        x2 = x1: y2 = y1
        DEADSHAFT x1, y1, n
        IF x1 <> x2 OR y1 <> y2 THEN STOP
        IF n < 5 THEN mz(x, y) = SOIL: mz(x1, y1) = SOIL
        EXIT SUB
      END IF
      c = c + 1
    END IF
  LOOP UNTIL done OR (RND < .1 AND c > 4) OR c > 30
END SUB

SUB DIGMAZE
  minw = 2
  maxw = 14
  minh = 2
  maxh = 9
  NRECTS = RAND(2, 5)
  CLEARMAZE
  FOR i = 1 TO NRECTS
    n = 0
    DO
      w = RAND(minw, maxw)
      h = RAND(minh, maxh)
      x = RAND(1, MZW - w)
      y = RAND(1, MZH - h)
      rct(i).x1 = x
      rct(i).y1 = y
      rct(i).x2 = rct(i).x1 + w
      rct(i).y2 = rct(i).y1 + h
      retry = FALSE
      FOR j = 1 TO i - 1
        IF NOT RECTSVALID(i, j) THEN
          retry = TRUE
          EXIT FOR
        END IF
      NEXT
      n = n + 1
    LOOP WHILE retry OR n > 10000
  NEXT
  DIM r(1 TO NRECTS, 1 TO NRECTS)
  FOR i = 1 TO RAND(0, 7)
    r1 = RAND(1, NRECTS)
    r2 = RAND(1, NRECTS)
    r(r1, r2) = TRUE
    r(r2, r1) = TRUE
  NEXT
  FOR i = 1 TO NRECTS
    FOR x = rct(i).x1 TO rct(i).x2
      FOR y = rct(i).y1 TO rct(i).y2
        IF x = rct(i).x1 OR x = rct(i).x2 OR y = rct(i).y1 OR y = rct(i).y2 THEN
          drw = TRUE
          FOR j = 1 TO NRECTS
            IF r(j, i) = TRUE THEN
              IF PTINRECT(j, x, y) THEN
                drw = FALSE
              END IF
            END IF
          NEXT
          IF drw THEN mz(x, y) = i + 100
        END IF
      NEXT
    NEXT
  NEXT
  FOR x = 1 TO MZW
    FOR y = 1 TO MZH
      IF mz(x, y) > 100 THEN mz(x, y) = EMPTY
    NEXT
  NEXT
  EXIT SUB
  DRAWMAZE 1, 1
  FOR sh = 1 TO 30
    FOR j = 1 TO 100
      x = RAND(1, MZW)
      y = RAND(1, MZH)
      IF PATH(x, y) THEN
        x0 = x: y0 = y
        z = 0: DEADSHAFT x0, y0, z
        mz(x, y) = EMPTY
        IF z < 2 THEN DRAWMAZE 1, 1
        sh = sh + 5
        EXIT FOR
      END IF
    NEXT
  NEXT
  'DIM arr(100) AS XY, arrc
  'FOR sh = 1 TO 4
  '  done = FALSE
  '  z = 0
  '  DO
  '    x = RAND(1, MZW)
  '    y = RAND(1, MZH)
  '    z = z + 1
  '  LOOP UNTIL PATH(x, y) OR z > 100
  '  IF z <= 100 THEN done = TRUE
  '
  '  IF done THEN
  '    arrc = 0
  '    FOR i = 0 TO 3
  '      a = RND * 4
  '      FOR j = 1 TO a
  '        x1 = x + j * d(i).x
  '        y1 = y + j * d(i).y
  '        IF x1 > 0 AND y1 > 0 AND x1 <= MZW AND y1 <= MZH THEN
  '          IF PATH(x1, y1) THEN
  '            arrc = arrc + 1
  '            arr(arrc).x = x1
  '            arr(arrc).y = y1
  '          ELSE
  '            j = a + 1
  '          END IF
  '        END IF
  '      NEXT
  '    NEXT
  '    FOR i = 1 TO arrc: mz(arr(i).x, arr(i).y) = SOIL: NEXT
  '    mz(x, y) = SOIL
  '  END IF
  '  q = q + 1
  '  IF q > 100 THEN EXIT FOR
  'NEXT
END SUB

FUNCTION DIST! (a AS XY, b AS XY)
  DIST! = SQR((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)
END FUNCTION

SUB DRAWMAZE (x0, y0)
  LOCATE 1, 1
  FOR y = 1 TO MZH
    LOCATE y0 + y - 1, x0
    FOR x = 1 TO MZW
      SELECT CASE mz(x, y)
        CASE EMPTY: c = 8:    ch$ = "#"
        CASE SOIL: c = 6:     ch$ = ":"
        CASE GOLD: c = 14:    ch$ = "$"
        CASE DIGGER: c = 5:   ch$ = "d"
        CASE ENEM: c = 1:     ch$ = "e"
        CASE JEWEL: c = 2:    ch$ = "7"
        'CASE IS > 200: c = 10: ch$ = CHR$(mz(x, y) - 200 + ASC("0"))
        'CASE IS > 100: c = 9: ch$ = CHR$(mz(x, y) - 100 + ASC("0"))
      END SELECT
      'ch$ = "Z"
      IF COLORFUL THEN COLOR c
      PRINT ch$;
    NEXT
    PRINT
  NEXT
  COLOR 7, 0
'  DELAY .2
END SUB

SUB MAKELEV
  DO
    DIGMAZE
    n = 0
    FOR x = 1 TO MZW
      FOR y = 1 TO MZH
        IF mz(x, y) = EMPTY THEN n = n + 1
      NEXT
    NEXT
  LOOP UNTIL (n > 50 AND (n < 60 OR RND < .01)) OR nosel
  PLACEGOLD
  PLACEJEWELS
  PLACECHARS
END SUB

FUNCTION PATH (x, y)
  IF mz(x, y) = EMPTY THEN
    xe = (mz(x + 1, y) = EMPTY AND mz(x - 1, y) = EMPTY)
    xs = (mz(x + 1, y) = SOIL AND mz(x - 1, y) = SOIL)
    ye = (mz(x, y + 1) = EMPTY AND mz(x, y - 1) = EMPTY)
    ys = (mz(x, y + 1) = SOIL AND mz(x, y - 1) = SOIL)
    IF (xe OR ye) THEN PATH = TRUE ELSE PATH = FALSE
  ELSE
    PATH = FALSE
  END IF
END FUNCTION

'place digger and monsters
SUB PLACECHARS
DIM lst(1 TO MZW * MZH) AS XY
  lstc = 0
  FOR x = 1 TO MZW
    FOR y = 1 TO MZH
      IF mz(x, y) = EMPTY AND NOT PATH(x, y) THEN
        lstc = lstc + 1
        lst(lstc).x = x: lst(lstc).y = y
      END IF
    NEXT
  NEXT

  z = RAND(1, lstc): mz(lst(z).x, lst(z).y) = DIGGER
  DO
    z1 = RAND(1, lstc)
  LOOP UNTIL (z1 <> z) AND (DIST(lst(z), lst(z1)) > 5 OR RND < .04)
  mz(lst(z1).x, lst(z1).y) = ENEM
END SUB

SUB PLACEGOLD
DIM lst(1 TO MZW * MZH) AS XY, lstc, c
  FOR x = 1 TO MZW
    FOR y = 1 TO MZH - 2
      IF mz(x, y) = SOIL AND mz(x, y + 1) = SOIL THEN
        lstc = lstc + 1
        lst(lstc).x = x
        lst(lstc).y = y
      END IF
    NEXT
  NEXT
  goldc = RAND(GOLDMIN, GOLDMAX)
  IF goldc > lstc THEN c = lstc ELSE c = goldc
  FOR i = 1 TO c
    z = RAND(1, lstc)
    mz(lst(z).x, lst(z).y) = GOLD
    lst(z).x = lst(lstc).x: lst(z).y = lst(lstc).y
    lstc = lstc - 1
  NEXT
END SUB

SUB PLACEJEWELS
DIM lst(1 TO MZW * MZH) AS XY
  lstc = 0
  FOR x = 1 TO MZW
    FOR y = 1 TO MZH
      IF mz(x, y) = SOIL THEN
        lstc = lstc + 1
        lst(lstc).x = x: lst(lstc).y = y
      END IF
    NEXT
  NEXT
  jewc = RAND(JEWMIN, JEWMAX)
  IF jewc > lstc THEN jewc = lstc - RAND(0, lstc / 3)
  FOR i = 1 TO jewc
    z = RAND(1, lstc)
    mz(lst(z).x, lst(z).y) = JEWEL
    lst(z).x = lst(lstc).x: lst(z).y = lst(lstc).y
    lstc = lstc - 1
  NEXT
END SUB

FUNCTION PTINRECT (r, x, y)
  IF (x > rct(r).x1 AND x < rct(r).x2) AND (y > rct(r).y1 AND y < rct(r).y2) THEN PTINRECT = TRUE ELSE PTINRECT = FALSE
  'IF (x >= rct(r).x1 AND x <= rct(r).x2) AND (y >= rct(r).y1 AND y <= rct(r).y2) THEN PTINBORDER = TRUE ELSE PTINBORDER = FALSE
END FUNCTION

FUNCTION RAND (min, max)
  RAND = INT(RND * (max - min + 1)) + min
END FUNCTION

FUNCTION RECTSVALID (BYVAL r1, BYVAL r2)
  res = TRUE
  IF rct(r1).y1 > rct(r2).y1 THEN ymin = rct(r1).y1 ELSE ymin = rct(r2).y1
  IF rct(r1).y2 < rct(r2).y2 THEN ymax = rct(r1).y2 ELSE ymax = rct(r2).y2
  IF ymin < ymax THEN
    IF rct(r1).x1 > rct(r2).x1 THEN SWAP r1, r2
    IF rct(r1).x1 = rct(r2).x1 - 1 THEN res = FALSE
    IF rct(r1).x2 = rct(r2).x1 - 1 THEN res = FALSE
    IF rct(r1).x2 < rct(r2).x1 THEN SWAP r1, r2
    IF rct(r1).x2 = rct(r2).x1 + 1 THEN res = FALSE
    IF rct(r1).x2 > rct(r2).x2 THEN SWAP r1, r2
    IF rct(r2).x2 = rct(r1).x2 + 1 THEN res = FALSE
  END IF
  IF rct(r1).x1 > rct(r2).x1 THEN xmin = rct(r1).x1 ELSE xmin = rct(r2).x1
  IF rct(r1).x2 < rct(r2).x2 THEN xmax = rct(r1).x2 ELSE xmax = rct(r2).x2
  IF xmin < xmax THEN
    IF rct(r1).y1 > rct(r2).y1 THEN SWAP r1, r2
    IF rct(r1).y1 = rct(r2).y1 - 1 THEN res = FALSE
    IF rct(r1).y2 = rct(r2).y1 - 1 THEN res = FALSE
    IF rct(r1).y2 < rct(r2).y1 THEN SWAP r1, r2
    IF rct(r1).y2 = rct(r2).y1 + 1 THEN res = FALSE
    IF rct(r1).y2 > rct(r2).y2 THEN SWAP r1, r2
    IF rct(r2).y2 = rct(r1).y2 + 1 THEN res = FALSE
  END IF
  RECTSVALID = res
END FUNCTION

SUB STORELEV (f, idx$)
  PRINT #f, "_mine #" + idx$
  IF BGR THEN c = RAND(0, 3) * 16 + RAND(0, 7) ELSE c = 17
  PRINT #f, "_levcol " + LTRIM$(RTRIM$(STR$(c)))
  FOR y = 1 TO MZH
    FOR x = 1 TO MZW
      SELECT CASE mz(x, y)
        CASE EMPTY: PRINT #f, "#";
        CASE SOIL: PRINT #f, ":";
        CASE GOLD: PRINT #f, "$";
        CASE DIGGER: PRINT #f, "d";
        CASE ENEM: PRINT #f, "e";
        CASE JEWEL: PRINT #f, "7";
        CASE IS > 100: PRINT #f, "#";
      END SELECT
    NEXT
    PRINT #f, ""
  NEXT
  PRINT #f, ""
END SUB

