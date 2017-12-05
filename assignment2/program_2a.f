      PROGRAM PROGRAM_2A
        INTEGER i, n
        REAL ti, tf, k, xAf, f, t
        READ(*, *) xAf
        READ(*, *) tf
        WRITE(*, *) 'xAf =', xAf, 'ti =', tf, 'のとき'
        k = 0.00036
        n = 0
        t = LOG(1 / (1 - xAf)) / k
200     n = n + 1
        ti = tf
        f = exp(-k * ti)
        tf = ti - (xAf - 1 + f) / (-k * f)
        IF (ABS(tf - t) .GE. 0.001) THEN
          GO TO 200
        ENDIF
        WRITE(*, *) 'tf =', tf
        WRITE(*, *) '解析解は t =', t
        WRITE(*, *) '操作回数', n, '回'
        STOP
      END
