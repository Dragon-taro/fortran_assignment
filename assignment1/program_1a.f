      PROGRAM PROGRAM_1A
        REAL xA, f

100     READ(*, *) xA
        IF(xA .GE. 0.) THEN
          f = 1 / (1 - xA)
          WRITE(*, *) 'xA =', xA, 'のとき  ', 'f(xA) =', f
          GO TO 100
        ENDIF
        STOP
      END
