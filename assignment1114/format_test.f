      PROGRAM VAPOR
        REAL Q, INT_T, FIN_T
        INT_T = 298.15
        FIN_T = 303.15
        OPEN(1, FILE='out.dat')
100     Q = 3.470 * (FIN_T - INT_T)
        Q = Q + 1.450E-3 * 0.5 * (FIN_T ** 2 - INT_T ** 2)
        Q = Q - 0.121E5 * (1 / FIN_T - 1 / INT_T)
        Q = Q * 8.314
        WRITE(1, 300) "Tf = ", FIN_T - 273.15
        WRITE(1, 400) "Q = ", Q
        IF (FIN_T .GE. 373.15) GO TO 200
        FIN_T = FIN_T + 5.
        GO TO 100
200     CONTINUE
        CLOSE(1)
300     FORMAT(A5, F6.2)
400     FORMAT(A4, E10.3/)
        STOP
      END
