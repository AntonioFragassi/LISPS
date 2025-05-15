;;Rotinas para cálculo de Kappa, Convergência, Declinação, Variaçao e Transformação UTM-GEO
;;Desenvolvida por Fragassi - Ultima Alteração: 15/11/07
;;Bugs arrumados:
;;1 - Calculo da convergencia estava sempre negativo idependente da posição em relação ao MC
;;	Coreção por Fragassi em 12/11/07
;;2 - Calculo da coordenada Geográfica estava sendo influenciada pela direção do angulo. Setado ANGDIR = 0, para efetuar cálculo correto
;;	Coreção por Fragassi em 15/11/07
(defun ACOS (COS_ANG / RES)
   (setq RES (ATAN (EXPT (/ (- 1 (* COS_ANG COS_ANG)) (* COS_ANG COS_ANG) ) 0.5)))
)

(defun Leroy ( / )
  
	(initget 7)
	(setq esc (getint "Forneça a escala do desenho: "))
	(setq  esc (/ esc 1000.0))
	(initget 7)
	(setq regua (getint "Forneça a régua do desenho: "))
	(command "_.STYLE" "ROMANS" "ROMANS" "0" "1.0" "0" "" "" "")
	(cond
	     	((= regua 40) (setq Ht 1.0))
	     	((= regua 50) (setq Ht 1.3))
	     	((= regua 60) (setq Ht 1.6))
	     	((= regua 80) (setq Ht 2.0))
	   	((= regua 100) (setq Ht 2.5))
	        ((= regua 120) (setq Ht 3.0))
	        ((= regua 140) (setq Ht 3.5))
	        ((= regua 175) (setq Ht 4.0))
	        ((= regua 200) (setq Ht 4.5))
	        ((= regua 240) (setq Ht 6.0))
	        ((= regua 290) (setq Ht 7.5))
	        ((= regua 350) (setq Ht 9.0))
	        ((= regua 425) (setq Ht 11.0))
	        ((= regua 425) (setq Ht 13.0))
	        (T otherwise (setq Ht 1.0))
	)
	(setq Ht (* Ht esc))
	(setvar "textsize" Ht)
);;fim da função leroy

(defun Const_elip ( / )
    (setq Ko 0.9996);;kapa do meridiano central
    (setq S1Seg 0.000004848136811076);;seno de 1 segundo
    (setq Ro0 (/ 180 pi))
    (setq Elipsoide (Getstring "\nQual sistema? <SAD69>; <WGS84>; <SIRGAS>; <SICAD> : "))
    (cond
    	((or (= Elipsoide "SAD69") (= Elipsoide "sad69"))
	    (progn
	    	(setq Achata (/ 1 298.25))
	    	(setq EixoMaior 6378160.00)
	    	(setq EixoMenor (- EixoMaior (* Achata EixoMaior)))
	    	(setq Exentricidade (SQRT(- (* Achata 2) (EXPT Achata 2))))
	    	(setq Exentricidade1 (SQRT(/(EXPT Exentricidade 2) (- 1 (EXPT Exentricidade 2)))))
	    );;fim do progn
    	);;fim do primeiro cond

	((or (= Elipsoide "WGS84") (= Elipsoide "wgs84"))
	    (progn
	    	(setq Achata (/ 1 298.257223563))
	    	(setq EixoMaior 6378137.00)
	    	(setq EixoMenor (- EixoMaior (* Achata EixoMaior)))
	    	(setq Exentricidade (SQRT(- (* Achata 2) (EXPT Achata 2))))
	    	(setq Exentricidade1 (SQRT(/(EXPT Exentricidade 2) (- 1 (EXPT Exentricidade 2)))))
	    );;fim do progn
    	);;fim do primeiro cond

	((or (= Elipsoide "SIRGAS") (= Elipsoide "sirgas"))
	    (progn
	    	(setq Achata (/ 1 298.257222101))
	    	(setq EixoMaior 6378137.00)
	    	(setq EixoMenor (- EixoMaior (* Achata EixoMaior)))
	    	(setq Exentricidade (SQRT(- (* Achata 2) (EXPT Achata 2))))
	    	(setq Exentricidade1 (SQRT(/(EXPT Exentricidade 2) (- 1 (EXPT Exentricidade 2)))))
	    );;fim do progn
    	);;fim do primeiro cond
	((or (= Elipsoide "SICAD") (= Elipsoide "sicad"))
	    (progn
	    	(setq Achata (/ 1 297.00))
	    	(setq EixoMaior 6378388.00)
	    	(setq EixoMenor (- EixoMaior (* Achata EixoMaior)))
	    	(setq Exentricidade (SQRT(- (* Achata 2) (EXPT Achata 2))))
	    	(setq Exentricidade1 (SQRT(/(EXPT Exentricidade 2) (- 1 (EXPT Exentricidade 2)))))
	    );;fim do progn
    	);;fim do primeiro cond
    );;fim do cond

    (setq KteA (+ 1 (* (/ 3.0 4) (EXPT Exentricidade 2 )) (* (/ 45.0 64) (EXPT Exentricidade 4)) (* (/ 175.0 256) (EXPT Exentricidade 6))
		  (* (/ 11025.0 16384) (EXPT Exentricidade 8)) (* (/ 43659.0 65536) (EXPT Exentricidade 10))))
    (setq KteB (+ (* (/ 3.0 4) (EXPT Exentricidade 2)) (*(/ 15.0 16) (EXPT Exentricidade 4)) (*(/ 525.0 512) (EXPT Exentricidade 6))
		  (*(/ 2205.0 2048) (EXPT Exentricidade 8)) (*(/ 72765.0 65536) (EXPT Exentricidade 10))))
    (setq KteC (+ (* (/ 15.0 64) (EXPT Exentricidade 4)) (* (/ 105.0 256) (EXPT Exentricidade 6)) (* (/ 2205.0 4096) (EXPT Exentricidade 8))
		  (* (/ 10395.0 16384) (EXPT Exentricidade 10))))
    (setq KteD (+ (*(/ 35.0 512) (EXPT Exentricidade 6)) (*(/ 315.0 2048) (EXPT Exentricidade 8)) (*(/ 31185.0 131072) (EXPT Exentricidade 10))))
    (setq KteE (+ (*(/ 315.0 16384) (EXPT Exentricidade 8)) (*(/ 3465.0  65536) (EXPT Exentricidade 10))))
    (setq KteF (*(/ 639.0 131072) (EXPT Exentricidade 10)))
    (setq KteK (* EixoMaior (- 1 (EXPT Exentricidade 2))))
    (setq Alfa (/(* KteA KteK) Ro0))
    (setq Beta (/ (* KteB KteK) 2))
    (setq Gama (/ (* KteC KteK) 4))
    (setq Delta (/ (* KteD KteK) 6))
    (setq Epsilon (/ (* KteE KteK) 8))
    (setq Psi (/ (* KteF KteK) 10))
);;fim da função


(defun Convergencia_UTM (CoordE CoordN MC / Res N1 FGeod Hemi Kt1 Kt2 Kt3 )
    (setq Hemi "S")
    ;;(setq E1 (abs(- CoordE 500000))) Correção 1
    (setq E1 (- CoordE 500000))
    (If (< CoordN 10000000.00) 
            (setq N1 (- 10000000.00 CoordN))
            (setq N1 (+ 10000000.00 CoordN))
    )
  
    (setq FGeod (Latitude_Geodesica N1 Hemi))
    (Parametros_Variaveis FGeod)
    (setq T1 (/ (sin FGeod ) (cos FGeod)))
    (setq Ni1 (* Exentricidade1 (cos FGeod)))
    (setq Kt1 (/(* T1 E1)(* GNormal S1Seg Ko)))
    (setq Kt2 (/ (* T1 (+ 1 (expt T1 2) (* -1.0 (expt Ni1 2)) (* -2.0 (expt Ni1 4))) (expt E1 3) )
		 (* 3 (expt GNormal 3) S1Seg (expt Ko 3))))
    (setq Kt2 (* -1.0 Kt2))
    (setq Kt3 (/ (* T1 (+ 2 (* 5.0 (expt T1 2)) (* 3.0 (expt T1 4)) ) (expt E1 5) )
		 (* 15.0 (expt GNormal 5) S1Seg (expt Ko 5))
	      )
    )
    (setq Res (/ (/ (+ Kt1 Kt2 Kt3) 3600) Ro0))
)


(defun Declinacao (ALAT ALON / Data Aux Ano Dia Mes P1 P2 P3 P4 P5 P6 P7 CD ELED VD XLAT XLON EPOC RES CTEX)
   (setq RAD  (/ PI 180.0))
   (setq CD   (list 0.0 -14.021375662 -0.168692826 -0.728121347 -0.152403506 0.009284154 0.010645054 0.003762631 0.006069034
 0.000997887 0.000660849 0.000228488 0.000147266 0.000138951 0.000102790 0.000077382 0.000033104 -0.000080797
 0.000081799 0.000023869 -0.000003334 -0.000001624 -0.000002092 0.000000783 -0.000000687 -0.000000467 -0.000003224
 0.000001136 -0.000000597 -0.000000384 -0.000000734 0.000000313))
  (princ "Latidude:")
  (print ALAT)
  (princ "Longitude:")
  (print ALON)
  (setq Aux (getreal "Forneça a data no seguinte formato: <ano.mesdia - 20160815>: "))
  ;;(setq Aux (getvar "CDATE"))
  (setq Ano (Fix (/ Aux 10000) ))
  (setq Mes (fix (/ (- Aux (* Ano 10000)) 100)))
  (setq Dia (fix (- Aux (+(* Ano 10000) (* Mes 100)))))
  (cond
    ((= Mes 1) (setq Data Dia));; Mes de janeiro
    ((= Mes 2) (setq Data (+ 31 Dia)));; Mes de fevereiro
    ((= Mes 3) (setq Data (+ 31 28 Dia)));; Mes de março
    ((= Mes 4) (setq Data (+ 31 28 31 Dia)));; Mes de abril
    ((= Mes 5) (setq Data (+ 31 28 31 30 Dia)));; Mes de maio
    ((= Mes 6) (setq Data (+ 31 28 31 30 31 Dia)));; Mes de junho
    ((= Mes 7) (setq Data (+ 31 28 31 30 31 30 Dia)));; Mes de julho
    ((= Mes 8) (setq Data (+ 31 28 31 30 31 30 31 Dia)));; Mes de agosto
    ((= Mes 9) (setq Data (+ 31 28 31 30 31 30 31 31 Dia)));; Mes de setembro
    ((= Mes 10) (setq Data (+ 31 28 31 30 31 30 31 31 30 Dia)));; Mes de outubro
    ((= Mes 11) (setq Data (+ 31 28 31 30 31 30 31 31 30 31 Dia)));; Mes de novembro
    ((= Mes 12) (setq Data (+ 31 28 31 30 31 30 31 31 30 31 30 Dia)));; Mes de dezembro
   )
   (setq Aux (/ Data 365.0))
   (setq EPC (- (+ Ano Aux) 1990))
      	(setq LAT (+ ALAT  15.0))                                          
      	(setq LONG (+ ALON  55.0))
;-----------------------------------------------------------------
       	(setq P1 (+(NTH 1 CD)(*(NTH 2 CD)LAT)(*(NTH 3 CD)LONG)(*(NTH 4 CD)EPC)(*(NTH 5 CD)LONG LONG)(*(NTH 6 CD)LONG LAT)(*(NTH 7 CD)LONG EPC)(*(NTH 8 CD)LAT LAT)))
      	(setq P2 (+(* (NTH 9 CD) LAT EPC) (* (NTH 10 CD) EPC EPC) (* (NTH 11 CD) LONG LONG LONG) (* (NTH 12 CD) LONG LONG LAT) (* (NTH 13 CD) LONG LONG EPC)))
  	(setq P3 (+(* (NTH 14 CD) LONG LAT LAT) (* (NTH 15 CD) LONG LAT EPC) (* (NTH 16 CD) LONG EPC EPC) (* (NTH 17 CD) LAT LAT LAT) (* (NTH 18 CD) LAT LAT EPC)))
  	(setq P4 (+(* (NTH 19 CD) LAT EPC EPC) (* (NTH 20 CD) LONG LONG LONG LONG) (* (NTH 21 CD) LONG LONG LONG LAT) (* (NTH 22 CD) LONG LONG LONG EPC)))
  	(setq P5 (+(* (NTH 23 CD) LONG LONG LAT LAT) (* (NTH 24 CD) LONG LONG LAT EPC) (* (NTH 25 CD) LONG LONG EPC EPC) (* (NTH 26 CD) LONG LAT LAT LAT)))
  	(setq P6 (+(* (NTH 27 CD) LONG LAT LAT EPC) (* (NTH 28 CD) LAT LAT LAT EPC) (* (NTH 29 CD) LONG LAT EPC EPC)))
     	(setq P7 (+(* (NTH 30 CD) LAT LAT LAT LAT)(* (NTH 31 CD) LAT LAT EPC EPC)))
      	(setq ELED (+ P1 P2 P3 P4 P5 P6 P7))
        (setq Res (ABS(* ELED RAD)))
        (IF (< ELED 0)
	  	;(setq Aux (STRCAT "Declinação: -" (ANGTOS Res 1 4)))
        	;(setq Aux (STRCAT "Declinação: " (ANGTOS Res 1 4))))
	  	(setq Aux (STRCAT "Declinação: -" (ANGTOS Res 1 2)))
        	(setq Aux (STRCAT "Declinação: " (ANGTOS Res 1 2))))

;-----------------------------------------------------------------
       	(setq P1 (+ (NTH 4 CD) (*(NTH 7 CD) LONG)))
     	(setq P2 (+ (*(NTH 9 CD) LAT) (* 2.0 (NTH 10 CD) EPC) (*(NTH 13 CD) LONG LONG)))
      	(setq P3 (+ (*(NTH 15 CD) LONG LAT) (* 2.0 (NTH 16 CD) LONG EPC) (*(NTH 18 CD) LAT LAT)))
      	(setq P4 (+ (* 2.0 (NTH 19 CD) LAT EPC) (*(NTH 22 CD) LONG LONG LONG)))
      	(setq P5 (+ (*(NTH 24 CD) LONG LONG LAT) (* 2.0 (NTH 25 CD) LONG LONG EPC)))
      	(setq P6 (+ (*(NTH 27 CD) LONG LAT LAT) (*(NTH 28 CD) LAT LAT LAT) (* 2.0 (NTH 29 CD) LONG LAT EPC) (* 2.0 (NTH 31 CD) LAT LAT EPC)))
      	(setq VD (* (+ P1  P2  P3  P4  P5  P6) 60.0))
        (setq Res (ABS(* (/ VD 60) RAD)))

        ;(IF (< VD 0)
	;    (setq RES (STRCAT " / Variação: -" (ANGTOS Res 1 4)));ORIGINAL
	;    (setq RES (STRCAT " / Variação: "  (ANGTOS Res 1 4)));ORIGINAL
	;  )

        (IF (< VD 0)
	    (setq RES (STRCAT " / Variação: -" (substr (ANGTOS Res 1 4) 3)));;ALTERADO PARA ATENDER O PROJETO IPECE
	    (setq RES (STRCAT " / Variação: "  (substr (ANGTOS Res 1 4) 3)));ALTERADO PARA ATENDER O PROJETO IPECE
	  )
        (setq RES (strcat Aux RES))
;-----------------------------------------------------------------

) 


(defun Calc_kappa (CoordE CoordN MC / Res  N1 Hemi FGeod)
    (setq Hemi "S")
    (If (< CoordN 10000000.00) 
            (setq N1 (- 10000000.00 CoordN))
            (setq N1 (+ 10000000.00 CoordN))
    )
    (setq E1 (abs(- CoordE 500000)))
    (setq FGeod (Latitude_Geodesica N1 Hemi))
    (setq Res (* Ko (+ 1 (/ (expt E1 2) (* 2.0 (expt RMedio 2))) (/ (expt E1 4) (* 24.0 (expt RMedio 4))))))
)

(defun Parametros_Variaveis (Fi)
    (setq GNormal ( / EixoMaior (sqrt (- 1 (* (EXPT Exentricidade 2) (EXPT (Sin Fi) 2))))))
    (setq PNormal (/ (* EixoMaior (- 1 (EXPT Exentricidade 2))) (sqrt (- 1 (* (EXPT Exentricidade  2) (EXPT (Sin Fi) 2))))))
    (setq RCSMeridiana (/ (* EixoMaior (- 1 (EXPT Exentricidade 2))) (SQRT (EXPT (- 1 (* (EXPT Exentricidade 2) (EXPT (Sin Fi) 2))) 3)))) 
    (setq RMedio ( sqrt(* GNormal RCSMeridiana)))
    (setq RParalelo (* GNormal (Cos Fi)))
)

(defun Latitude_Geodesica (N1 Hemi / Fi L TT Alf1 Alf2 Alf3 Alf4 Alf5 Bet1 Bet2 Bet3 Bet4 Bet5 DFi)
    (setq Fi (/ N1 (* ko Alfa Ro0)))
    (if (= Hemi "S")
        (setq Fi (* -1 Fi))
    )
    (setq L (+ (* Alfa ro0) (* -2.0 Beta (cos (* 2.0 Fi))) (* 4.0 Gama (cos (* 4.0 Fi))) (* -6.0 Delta (cos (* 6.0 Fi)))
	       (* 8.0 Epsilon (cos (* 8.0 Fi))) (* -10.0 Psi (cos (* 10.0 Fi))) ))
    (setq TT (* (/ 1.0 L) (+ (* Beta (sin (* 2.0 Fi))) (* -1.0 Gama (sin (* 4.0 Fi))) (* Delta (sin (* 6.0 Fi)))
	       (* -1.0 Epsilon (sin (* 8.0 Fi))) (* Psi (sin (* 10.0 Fi))) ) ))
    (setq alf1 (* (/ -1.0 L) (+ (* 2.0 beta  (sin (* 2.0 Fi))) (* -8.0 Gama  (sin (* 4.0 Fi))) (* 18.0 Delta  (sin (* 6.0 Fi))))))
    (setq Alf2 (* (/ -1.0 L) (+ (* (/ 4.0 3) beta  (cos (* 2.0 Fi))) (* (/ -32.0 3) Gama  (cos (* 4.0 Fi))) (* 36.0 Delta  (cos (* 6.0 Fi))))))
    (setq Alf3 (* (/ -1.0 L) (+ (* (/ 2.0 3) beta  (sin (* 2.0 Fi))) (* (/ 32.0 3) Gama  (sin (* 4.0 Fi))) (* -54.0  Delta  (sin (* 6.0 Fi))))))
    (setq Alf4 (* (/ -1.0 L) (+ (* (/ 4.0 15) beta  (cos (* 2.0 Fi))) (* (/ 128.0 15) Gama  (cos (* 4.0 Fi))) (* (/ -324.0 5) Delta  (cos (* 6.0 Fi))))))
    (setq Alf5 (* (/ -1.0 L) (+ (* (/ 4.0 15) beta  (sin (* 2.0 Fi))) (* (/ -256.0 45) Gama  (sin (* 4.0 Fi))) (* (/ 324.0 5)  Delta  (sin (* 6.0 Fi))))))
    (setq Bet1 Alf1)
    (setq Bet2 (+ Alf2 (* 2.0 Alf1 Alf1)))
    (setq Bet3 (+ Alf3 (* 5.0 Alf1 Alf2) (* 5.0 (expt Alf1 3))))
    (setq Bet4 (+ Alf4 (* 6.0 Alf1 Alf3) (* 3.0 Alf2 Alf2) (* 21.0 Alf1 Alf1 Alf2) (* 14.0 (expt Alf1 4))))
    (setq Bet5 (+ Alf5 (* 7.0 Alf1 Alf4) (* 7.0 Alf2 Alf3) (* 28.0 Alf1 Alf1 Alf3) (* 28.0 Alf1 Alf2 Alf2)
		  (* 84.0 Alf1 Alf1 Alf1 Alf2) (* 42.0 (expt Alf1 5))))
    (setq Dfi (+ TT (* Bet1 (expt TT 2)) (* Bet2 (expt TT 3)) (* Bet3 (expt TT 4)) (* Bet4 (expt TT 5 )) (* Bet5 (expt TT 6))))
    (setq Fi (+ Fi Dfi))
)



(defun UTM_GEO (X_E Y_N MC Hemi / MC Hemi Lat Long Res FGeod N1 E1 T1 Ni1 Kt1 Kt2 Kt3 )
    (setq MC (/(* MC Pi) 180.0))
    (setq N1 (- 10000000 Y_N))
    (setq E1 (- X_E 500000))
    (setq FGeod (Latitude_Geodesica N1 Hemi))
    (Parametros_Variaveis FGeod)
    (setq T1 (/ (sin FGeod ) (cos FGeod)))
    (setq Ni1 (* Exentricidade1 (cos FGeod)))
    (setq Kt1 (/ (* T1 (+ 1.0 (expt Ni1 2)) (expt E1 2))
		 (* 2.0 (expt GNormal 2) S1Seg (expt Ko 2))
	      )
    )
    (setq Kt1 (* -1.0 Kt1))
    (setq Kt2 (/ (* T1 (+ 5 (* 3 (expt T1 2)) (* 6 (expt Ni1 2)) (* -6 (expt Ni1 2) (expt T1 2))
			  (* -3 (expt Ni1 4)) (* -9 (expt Ni1 4) (expt T1 2))) (expt E1 4) )
	         (* 24 (expt GNormal 4) S1Seg (expt Ko 4))
	      )
    )
    (setq Kt3 (/ (* T1 (+ 61.0 (* 90.0 (expt T1 2)) (* 45.0 (expt T1 4)) (* 107.0 (expt Ni1 2)) (* -162.0 (expt Ni1 2) (expt T1 2))
		 (* -45.0 (expt T1 4) (expt Ni1 2))) (expt E1 6))
	         (* 720.0 (expt GNormal 6) S1Seg (expt Ko 6))
	      )
    )
    (setq Kt3 (* -1.0 Kt3))
    (setq Lat (+ FGeod (/ (/ (+ Kt1 Kt2 Kt3) 3600) Ro0)))
    (print "Valor de LAT na Função UTM_GEO : ")
    (princ Lat )
    (setq Kt1 (/ E1  (* GNormal (cos FGeod) S1Seg  Ko)))
    (setq Kt2 (/ (* (+ 1 (* 2(expt T1 2)) (expt Ni1 2) ) (expt E1 3) )
		 (* 6 (expt GNormal 3) (cos FGeod) S1Seg (expt Ko 3))))
    (setq Kt2 (* -1.0 Kt2))
    (setq Kt3 (/ (* (+ 5.0 (* 28.0 (expt T1 2)) (* 24.0 (expt T1 4)) (* 6.0 (expt Ni1 2)) (* 8.0 (expt Ni1 2) (expt T1 2))) (expt E1 5) )
		 (* 120.0 (expt GNormal 5) (cos FGeod) S1Seg (expt Ko 5))
	      )
    )
    (setq Long  (/ (/ (+ Kt1 Kt2 Kt3) 3600) Ro0))
    (If (< E1 500000)
            (setq Long (+  MC Long))
            (setq Long (- MC Long))
    )
    (setq Res (list Lat Long ))

)



(defun c:UTM (/ Aux Flag EE NN MC Longitude Latitude K0 S1Seg EixoMaior EixoMenor Exentricidade Exentricidade1 GNormal PNormal RCSMeridiana RMedio RParalelo Convergencia KteXVIII KAPPA DECLIN)
    (setq oldangbase (getvar "angbase"));;correção 2
    (setvar "angbase" 0.0)
    (setq oldangdir (getvar "angdir"));;correção 2
    (setvar "angdir" 0)
    (setq oldosmode (getvar "osmode"))
    (setvar "osmode" 0)
    (setq oldcmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (Leroy)
    (setq MC (getreal "\nForneça o MC, <-> a oeste e <+> a leste de Gr.:"))
    (setq Hemi "S")
    (Const_elip)
    (setq OPCAO (GETINT "\nQue deseja fazer? UTM/GEO <1>; CONVERGENCIA E ESCALA <2>; DECLINAÇÃO <3>; Tudo <4>: IPECE <5>:"))
    (COND
      	( (= OPCAO 1)
		(while (setq flag T)
                        (command "_.layer" "m" "TXT_MALHA_GEO" "")
			(setq PONTO (GETPOINT "\nClique num ponto:"))
			(setq EE (NTH 0 PONTO))
			(setq NN (NTH 1 PONTO))
		        (print EE)
		        (print NN)
	    		(setq Aux (UTM_GEO EE NN MC Hemi))
		        (setq Latitude (nth 0 Aux))
		        (setq Longitude (nth 1 Aux))
		        ;(if (< Latitude 0 )  (setq Latitude (strcat "-" (angtos (abs Latitude) 1 6)) )
        		 ;   (setq Latitude (angtos (abs Latitude) 1 6))

		         (setq Latitude (strcat "-"(angtos (abs Latitude) 1 7)))

		        ;(if (< Longitude 0 )  (setq Longitude (strcat "-" (angtos (abs Longitude) 1 6)) )
        		;(setq Longitude (angtos (abs Longitude) 1 6)))

		        (setq Longitude (strcat "-"(angtos (abs Longitude) 1 7)))
		        (princ "\nOnde quer escrever a Latitude?")
		        (command "_.text"  pause "" pause Latitude)
		        (princ "\nOnde quer escrever a Longitude?")
		        (command "_.text"  pause "" "" Longitude)
	  	);;fin do while
	);;fim do cond 1

      	( (= OPCAO 2)
		(while (setq flag T)
			(setq PONTO (GETPOINT "\nClique num ponto:"))
			(setq EE (NTH 0 PONTO))
			(setq NN (NTH 1 PONTO))
	    		(setq Convergencia (Convergencia_UTM EE NN MC))
	    		(setq KAPPA (Calc_kappa EE NN MC))
    			(if (< Convergencia 0 )  (setq Convergencia (strcat "-" (angtos (abs Convergencia) 1 6)) )
        		(setq Convergencia (angtos (abs Convergencia) 1 6)))
		        (princ "\nOnde quer escrever a Convergência?")
		        (command "_.text"  pause "" pause Convergencia)
		        (princ "\nOnde quer escrever o Kappa?")
		        (command "_.text"  pause "" "" (rtos kappa 2 7))

	  	);;fin do progn
	);;fim do cond 2

      	( (= OPCAO 3)
		(while (setq flag T)
			(setq PONTO (GETPOINT "\nClique num ponto:"))
			(setq EE (NTH 0 PONTO))
			(setq NN (NTH 1 PONTO))
		        (setq Aux (UTM_GEO EE NN MC Hemi))
		  	(setq Latitude (nth 0 Aux))
		        (setq Longitude (nth 1 Aux))
		        (setq Latitude (/(* (nth 0 Aux) 180)pi))
		        (setq Longitude (/(* (nth 1 Aux) 180)pi))
		        (setq Aux (Declinacao Latitude Longitude))
		        (princ "\nOnde quer escrever a Declinação e a Variação?")
		        (command "_.text"  pause "" pause Aux)
		        
	  	);;fin do progn
	);;fim do cond 3


      	( (= OPCAO 4)
		(while (setq flag T)
			(setq PONTO (GETPOINT "\nClique num ponto:"))
			(setq EE (NTH 0 PONTO))
			(setq NN (NTH 1 PONTO))
	    		(setq Aux (UTM_GEO EE NN MC Hemi))
		        (setq Latitude (nth 0 Aux))
		        (setq Longitude (nth 1 Aux))
		        ;(if (< Latitude 0 )  (setq Latitude (strcat "-" (angtos (abs Latitude) 1 6)) )
        		;    (setq Latitude (angtos (abs Latitude) 1 6)))
		        (setq Latitude (angtos (abs Latitude) 1 6))
		        ;(if (< Longitude 0 )  (setq Longitude (strcat "-" (angtos (abs Longitude) 1 6)) )
        		;(setq Longitude (angtos (abs Longitude) 1 6)))
		        (setq Longitude (angtos (abs Longitude) 1 6))
		        (princ "\nOnde quer escrever a Latitude?")
		        (command "_.text"  pause "" pause Latitude)
		        (princ "\nOnde quer escrever a Longitude?")
		        (command "_.text"  pause "" "" Longitude)
		  	(setq Convergencia (Convergencia_UTM EE NN MC))
	    		(setq KAPPA (Calc_kappa EE NN MC))
    			(if (< Convergencia 0 )  (setq Convergencia (strcat "-" (angtos (abs Convergencia) 1 6)) )
        		(setq Convergencia (angtos (abs Convergencia) 1 6)))
		        (princ "\nOnde quer escrever a Convergência?")
		        (command "_.text"  pause "" "" Convergencia)
		        (princ "\nOnde quer escrever o Kappa?")
		        (command "_.text"  pause "" "" (rtos kappa 2 7))
		  	(setq Latitude (/(* (nth 0 Aux) 180)pi))
		        (setq Longitude (/(* (nth 1 Aux) 180)pi))
		        (setq Aux (Declinacao Latitude Longitude))
		        (princ "\nOnde quer escrever a Declinação e a Variação?")
		        (command "_.text"  pause "" "" Aux)
		  
	  	);;fin do progn
	);;fim do cond 3

      	( (= OPCAO 5)
		(while (setq flag T)
			(setq PONTO (GETPOINT "\nClique num ponto:"))
			(setq EE (NTH 0 PONTO))
			(setq NN (NTH 1 PONTO))
	    		(setq Aux (UTM_GEO EE NN MC Hemi))
		        (setq Latitude (nth 0 Aux))
		        (setq Longitude (nth 1 Aux))
		  	(setq Convergencia (Convergencia_UTM EE NN MC))
	    		(setq KAPPA (Calc_kappa EE NN MC))
    			(if (< Convergencia 0 )  (setq Convergencia (strcat "-" (angtos (abs Convergencia) 1 4)) )
        		(setq Convergencia (angtos (abs Convergencia) 1 4)))
		        (princ "\nOnde quer escrever a Convergência?")
		        (command "_.text"  pause "" "" Convergencia)
		        (princ "\nOnde quer escrever o Kappa?")
		        (command "_.text"  pause "" "" (rtos kappa 2 7))
		  	(setq Latitude (/(* (nth 0 Aux) 180)pi))
		        (setq Longitude (/(* (nth 1 Aux) 180)pi))
		        (setq Aux (Declinacao Latitude Longitude))
		        (princ "\nOnde quer escrever a Declinação e a Variação?")
		        (command "_.text"  pause "" "" Aux)
		  
	  	);;fin do progn
	);;fim do cond 3
    );;fim do cond
	(setvar "angdir" oldangdir)
	(setvar "osmode" oldosmode)
	(setvar "cmdecho" oldcmdecho)
)
(princ "\nDigite 'UTM' para iniciar")