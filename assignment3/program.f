      PROGRAM STATIC
        INTEGER N, I, k
        REAL DP(1:1000), DPMAX, DPMIN, AVE, SD, x_AVE, x_SD, g_AVE, g_SD
        REAL x
C        DP: データの配列, DPMAX: DPの最大値, DPMIN: DPの最小値, AVE: DPの平均, SD: DPの標準偏差
C        x_AVE: DPの自然対数の平均, x_SD: DPの自然対数の標準偏差
C        g_AVE: DPの幾何平均, g_SD: DPの幾何標準偏差
C        x: ヒストグラムの分割変数

C        ファイルのデータを読み込み配列に格納
        OPEN(1, FILE='diam.dat')
        I = 0
        N = 0
100     I = I + 1
        READ(1, *) DP(I)
        IF (DP(I) .GT. 0.) THEN
          N = N + 1
          GO TO 100
        ENDIF
        CLOSE(1)

C        初期値の設定
        DPMAX = DP(1)
        DPMIN = DP(1)
        AVE = DP(1)
        SD = DP(1) * DP(1)
C        **2 は遅いらしい？？
        x_AVE = LOG(DP(1))
        x_SD = LOG(DP(1)) * LOG(DP(1))

C        和と最大最小を求めるループ処理 + 標準偏差とかの後処理
        DO 200 I = 2, N
C          built in関数で簡略化
          DPMAX = max(DP(I), DPMAX)
          DPMIN = min(DP(I), DPMIN)
          AVE = AVE + DP(I)
          SD = SD + DP(I) * DP(I)
          x_AVE = x_AVE + LOG(DP(I))
          x_SD = x_SD + LOG(DP(I)) * LOG(DP(I))
200     CONTINUE
        AVE = AVE / N
        SD = SQRT(SD / N - AVE * AVE)
        x_AVE = x_AVE / N
        x_SD = SQRT(x_SD / N - x_AVE * x_AVE)
        g_AVE = EXP(x_AVE)
        g_SD = EXP(x_SD)

C        出力
        WRITE(*, 20) 'N = ', N
        WRITE(*, 10) 'DPMAX = ', DPMAX
        WRITE(*, 10) 'DPMIN = ', DPMIN
        WRITE(*, 10) '平均値は', AVE
        WRITE(*, 10) '標準偏差は', SD
        WRITE(*, 10) 'DPの自然対数の平均値は', x_AVE
        WRITE(*, 10) 'DPの自然対数の標準偏差は', x_SD
        WRITE(*, 10) 'DPの幾何平均値は', g_AVE
        WRITE(*, 10) 'DPの幾何標準偏差は', g_SD
        WRITE(*, 20) 'CASE1:', NCNT(AVE - SD, AVE, DP, N)
        WRITE(*, 10) 'rate:', REAL(NCNT(AVE - SD, AVE, DP, N)) / REAL(N)
        WRITE(*, 20) 'CASE2:', NCNT(AVE, AVE + SD, DP, N)
        WRITE(*, 10) 'rate:', REAL(NCNT(AVE, AVE + SD, DP, N)) / REAL(N)
        WRITE(*, 20) 'CASE3:', NCNT(g_AVE / g_SD, g_AVE, DP, N)
        WRITE(*, 10) 'rate:', REAL(NCNT(g_AVE / g_SD, g_AVE, DP, N))
     &   / REAL(N)
        WRITE(*, 20) 'CASE4:', NCNT(g_AVE, g_AVE * g_SD, DP, N)
        WRITE(*, 10) 'rate:', REAL(NCNT(g_AVE, g_AVE * g_SD, DP, N))
     &   / REAL(N)
10      FORMAT(A, F4.2)
20      FORMAT(A, I3)

C        ヒストグラム
        x = 0.
300     k = NCNT(x, x + 0.5, DP, N)
C        WRITE(*, 40) k
C        WRITE(*, 40) ('*', I = 1, k, 1)
        WRITE(*, 30) x, x + 0.5, k, ('*', I = 1, k, 1)
        x = x + 0.5
        IF (x .GT. DPMAX + 0.5) GO TO 400
        GO TO 300
400     CONTINUE
30      FORMAT(F3.1, '〜', F3.1, 1X, I3, ':', 30A1)
40      FORMAT(I3)


        STOP
      END

      INTEGER FUNCTION NCNT(DP0, DP1, DP, N)
        INTEGER N, I, NUM
        REAL DP0, DP1, DP(1:N)
        NUM = 0
        DO 100 I = 1, N
          IF (DP(I) .GE. DP0 .AND. DP(I) .LT. DP1) THEN
            NUM = NUM + 1
          ENDIF
100     CONTINUE
        NCNT = NUM
      END
