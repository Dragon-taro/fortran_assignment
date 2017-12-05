      PROGRAM PROGRAM_1B
        INTEGER i, n
        REAL S, dx, x, xAf, f, k
        xAf = 0.9
        k = 0.00036

        S = LOG(1 / (1 - xAf)) / k
        WRITE(*, *) '解析解'
        WRITE(*, *)  't =', S, 's'

        WRITE(*, *) '数値解'
200     READ(*, *) n
        x = 0.
        IF (n .GT. 0.) THEN
          dx = (xAf - 0) / REAL(n)
          WRITE(*, *) dx * n
          S = ((1. / (1. - 0.)) + (1. / (1. - xAf))) / 2
          DO 100, i = 1, n - 1
            x = x + dx
            f = 1. / (1. - x)
            S = S + f
            IF (i .GT. 9000000) THEN
C              WRITE(*, *) i, x, S
            ENDIF
100       CONTINUE
          S = S * dx / k
          WRITE(*, *) '分割数n =', n, 'のときの  ', 't =', S, 's'
          GO TO 200
        ENDIF
        STOP
      END
