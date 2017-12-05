      PROGRAM ASS4
        DOUBLE PRECISION LAGRA, LAG_A(1:5), P(1:5), P_SOW(1:5), Hm(1:5)
C        DOUBLE PRECISIONでこいつらを宣言しておかないとエラー
        DOUBLE PRECISION A_a, A_b, B_a, B_b, D_a, D_b
        DOUBLE PRECISION A_L(1:5), B_L(1:5), D_L(1:5)
C        最小二乗法による新たなA, B, D
        INTEGER N, I, J
        DOUBLE PRECISION T, A(1:5), B(1:5), D(1:5), Hm0(1:5)
        DOUBLE PRECISION A_6, B_6, D_6, Hm3, P_3, FHM_6, FCPM_6
C        (6)のA, B, Dと (3)のHmとそのときのP
        DOUBLE PRECISION FCPM, FHM

        A = (/2.9294, 1.9417, -0.35656, -3.0938, -5.5669/)
        B = (/1.9723E-3, 2.8469E-3, 4.8324E-3, 7.1474E-3, 9.1555E-3/)
        D = (/0.98003E5, 2.9143E5, 7.6851E5, 13.6033E5, 19.444E5/)
        P_SOW = (/101.325, 400., 1150., 2000., 2900./)
        P = (/101.325E3, 400.0E3, 1150.0E3, 2000.0E3, 2900.0E3/)
        Hm0 = (/53.600, 53.423, 52.930, 52.301, 51.535/)

C        (1)の表の表示
        T = 523.15
        WRITE(*, *) 'Cpm  [J/mol•K]'
        WRITE(*, 10)
        WRITE(*, 20) (P_SOW(I), I = 1, 5, 1)
        WRITE(*, 30) ('-', L = 1, 60, 1)
C        Tを50Kずつずらしてその温度における各圧力のFCPMを出力
        DO 300 I = 1, 9
          WRITE(*, 40) T, (FCPM(A(J), B(J), D(J), T), J = 1, 5, 1)
          T = T + 50.
300     CONTINUE
        WRITE(*, *)

C        (2)の表の表示
C        サブルーチンにしたらコードが簡略化できそう
        T = 523.15
        WRITE(*, *) 'Hm [kJ/mol]'
        WRITE(*, 10)
        WRITE(*, 20) (P_SOW(I), I = 1, 5, 1)
        WRITE(*, 30) ('-', L = 1, 60, 1)
C        Tを50Kずつずらしてその温度における各圧力のFCPMを出力
        DO 400 I = 1, 9
          WRITE(*, 40) T, (FHM(A(J), B(J), D(J), T, Hm0(J)),
     &      J = 1, 5, 1)
          T = T + 50.
400     CONTINUE
        WRITE(*, *)

10      FORMAT(6X, 'T', 26X, 'P[kPa]')
20      FORMAT(5X, '[K]', 2X, 5F10.2)
30      FORMAT(60A1)
40      FORMAT(F10.2, 5F10.4)

C        (3)
C       まずHmを格納した配列を作る
        T = 523.15
        DO 500 I = 1, 5
          Hm(I) = FHM(A(I), B(I), D(I), T, Hm0(I))
500     CONTINUE

C        LAG_Aを先に求めて高速化
        N = 5
        DO 100 I = 1, N
          LAG_A(I) = Hm(I)
          DO 200 J = 1, N
            IF (J .NE. I) THEN
              LAG_A(I) = LAG_A(I) / (P(I) - P(J))
            ENDIF
200       CONTINUE
100     CONTINUE
        P_3 = 1520.0E3
        Hm3 = LAGRA(N, P, Hm, P_3, LAG_A)
        WRITE(*, *) '1250kPa, 250℃において'
        WRITE(*, 50) 'Hm =', Hm3, 'kJ/mol'
50      FORMAT(A, F8.4, A)

C        (4) A, B, Dそれぞれの傾き、切片を求める
        CALL LSQ(N, P, A, A_a, A_b)
        WRITE(*, *) 'Aの傾き:', A_a, 'Aの切片:', A_b
        CALL LSQ(N, P, B, B_a, B_b)
        WRITE(*, *) 'Bの傾き:', B_a, 'Bの切片:', B_b
        CALL LSQ(N, P, D, D_a, D_b)
        WRITE(*, *) 'Dの傾き:', D_a, 'Dの切片:', D_b

C        (5)
C        まず、最小二乗法による新たなA, B, Dを求める
        DO 600 I = 1, 5
          A_L(I) = A_a * P(I) + A_b
          B_L(I) = B_a * P(I) + B_b
          D_L(I) = D_a * P(I) + D_b
600     CONTINUE
        T = 523.15
        WRITE(*, *) 'Cpm  [J/mol•K]'
        WRITE(*, 10)
        WRITE(*, 20) (P_SOW(I), I = 1, 5, 1)
        WRITE(*, 30) ('-', L = 1, 60, 1)
C        Tを50Kずつずらしてその温度における各圧力のFCPMを出力
        DO 700 I = 1, 9
          WRITE(*, 40) T, (FCPM(A_L(J), B_L(J), D_L(J), T), J = 1, 5, 1)
          T = T + 50.
700     CONTINUE
        WRITE(*, *)

        T = 523.15
        WRITE(*, *) 'Hm [kJ/mol]'
        WRITE(*, 10)
        WRITE(*, 20) (P_SOW(I), I = 1, 5, 1)
        WRITE(*, 30) ('-', L = 1, 60, 1)
C        Tを50Kずつずらしてその温度における各圧力のFCPMを出力
        DO 800 I = 1, 9
          WRITE(*, 40) T, (FHM(A_L(J), B_L(J), D_L(J), T, Hm0(J)),
     &      J = 1, 5, 1)
          T = T + 50.
800     CONTINUE
        WRITE(*, *)

C        (6)
        A_6 = A_a * P_3 + A_b
        B_6 = B_a * P_3 + B_b
        D_6 = D_a * P_3 + D_b
        T = 523.15
        WRITE(*, 60)
        WRITE(*, 70)
        WRITE(*, 80) ('-', L = 1, 30, 1)
C        Tを50Kずつずらしてその温度における各圧力のFCPMを出力
        DO 900 I = 1, 9
          FHM_6 = FHM(A_6, B_6, D_6, T, Hm3)
          FCPM_6 = FCPM(A_6, B_6, D_6, T)
          WRITE(*, 90) T, FCPM_6, FHM_6
          T = T + 50.
900     CONTINUE
        WRITE(*, *)
60      FORMAT(6X, 'T', 8X, 'Cpm', 8X, 'Hm')
70      FORMAT(5X, '[K]', 4X, '[J/mol•K]',2X, '[kJ/mol]')
80      FORMAT(30A1)
90      FORMAT(F10.2, 2F10.4)

        STOP
      END

C      (a)
      DOUBLE PRECISION FUNCTION FCPM(A, B, D, T)
        DOUBLE PRECISION A, B, D, T
        FCPM = (A + B * T + D / (T * T)) * 8.314
      END

C      (b)
      DOUBLE PRECISION FUNCTION FHM(A, B, D, T, H0)
        DOUBLE PRECISION A, B, D, T, T0, H0
        T0 = 523.15
        FHM = A * (T - T0) + B * (T * T - T0 * T0 ) / 2.
        FHM = FHM - D * (1. / T - 1. / T0)
        FHM = FHM * 8.314 * 10E-3 + H0
      END

C      (c)
      DOUBLE PRECISION FUNCTION LAGRA(N, X, Y, XX, A)
        DOUBLE PRECISION X(1:N), Y(1:N), XX, A(1:N)
        INTEGER N

        DOUBLE PRECISION BK
        INTEGER I, J

        LAGRA = 0.0
        DO 10 I = 1, N
C          既知データと一致する場合はそれを返して高速化
          IF (X(I) .EQ. XX) THEN
            LAGRA = Y(I)
            RETURN
          ENDIF
          BK = A(I)
          DO 20 J = 1, N
            IF (J .NE. I) THEN
              BK = BK * (XX - X(J))
            ENDIF
20       CONTINUE
        LAGRA = LAGRA + BK
10     CONTINUE
       RETURN
      END

      SUBROUTINE LSQ(N, X, Y, A, B)

        DOUBLE PRECISION X(N), Y(N), A, B
        INTEGER N

        DOUBLE PRECISION SX, SY, SXX, SXY
        INTEGER I

        SX = 0.
        SY = 0.
        SXX = 0.
        SXY = 0.
        DO 10 I = 1, N
          SX = SX + X(I)
          SY = SY + Y(I)
          SXX = SXX + X(I) * X(I)
          SXY = SXY + X(I) * Y(I)
10      CONTINUE
        A = (SX * SY - REAL(N) * SXY) / (SX * SX - REAL(N) * SXX)
        B = (SY - A * SX) / REAL(N)
        RETURN
      END
