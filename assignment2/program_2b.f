      PROGRAM PROGRAM_2B
        INTEGER n
        REAL T1, T2, T3, F1, F2, F3, xAf, k, t
        READ(*, *) xAf
100     READ(*, *) T2
        n = 0
        T1 = 0.0
        k = 0.00036
        F1 = xAf - 1.0 + exp(-k * T1)
        F2 = xAf - 1.0 + exp(-k * T2)
        IF(F1 * F2 .GT. 0) THEN
          WRITE(*, *) 'Initial values out of range!'
          GO TO 100
        ENDIF
        WRITE(*, *) '右端のt t =', T2, 'xAf =', xAf
300     CONTINUE
        n = n + 1
        T3 = (T1 + T2) / 2.
        F3 = xAf - 1 + exp(-k * T3)
        ERR = ABS(T1 - T2)
        IF(ERR .LE. 0.001) GO TO 200
        IF(F1 * F3 .GT. 0.) THEN
          T1 = T3
          F1 = xAf - 1 + exp(-k * T1)
        ELSE
          T2 = T3
          F2 = xAf - 1 + exp(-k * T2)
        ENDIF
        GO TO 300
200     CONTINUE
        WRITE(*, *) 't =', T3
        WRITE(*, *) '繰り返し回数', n, '回'
        t = LOG(1 / (1 - xAf)) / k
        WRITE(*, *) '解析解はt = ', t
        STOP
      END
