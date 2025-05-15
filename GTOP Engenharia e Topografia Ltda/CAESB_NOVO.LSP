(defun OCTANTE ( ANG / RESP )
    (cond
	((and (>= ANG 0.0)(< ANG 90.0)) (setq RESP ANG))
        ((and (>= ANG 90.0)(< ANG 180.0)) (setq RESP (- ANG 180.0)))
	((and (>= ANG 180.0)(< ANG 270.0)) (setq RESP (- ANG 180.0)))
	((and (>= ANG 270.0)(<= ANG 360.0)) (setq RESP (- ANG 360.0)))
    )
  (setq RESP RESP)
 )

(defun DADOS_BLOCO ( OBJETO / DADOS1 lista Attn Attv PONTO DESC COTA)
  (setq DADOS1 nil)
  (while (/= (cdr (assoc 0 (entget OBJETO))) "SEQEND")
        (setq OBJETO (entnext OBJETO))
  	(setq lista (entget OBJETO))
        (setq Attn (cdr (assoc 2 lista)))
  	(setq Attv (cdr (assoc 1 lista)))
        (cond
	  ((= Attn "PONTO" ) (setq PONTO Attv) )
          ((= Attn "DESC" ) (setq DESC Attv) )
          ((= Attn "COTA" ) (setq COTA Attv) )
	 )
    )  
  	(setq DADOS1 (append DADOS1 (list PONTO) (list DESC) (list COTA)))
        (setq DADOS1 DADOS1)
)

(defun DADOS_PV ( / p1 p2 p3 lista Paux ANG FLAG FLAG1 FLAG2 FLAG3 FLAG4 p1 TIPO TIPOCI) ;;
        (setq FLAG1 0 FLAG2 0 FLAG3 0 FLAG4 0)
        (if (/= COTA_DG1 nil) (setq FLAG1 1))
        (if (/= COTA_DG2 nil ) (setq FLAG2 2))
        (if (/= COTA_DG3 nil) (setq FLAG3 4))
        (if (/= COTA_DG4 nil) (setq FLAG4 8))
        (setq FLAG (+ FLAG1 FLAG2 FLAG3 FLAG4))
        (if (/= COTA_DG1 nil) (setq COTA_DG1 (rtos COTA_DG1 2 2)) (setq COTA_DG1 "") )
        (if (/= COTA_DG2 nil) (setq COTA_DG2 (rtos COTA_DG2 2 2)) (setq COTA_DG2 "") )
        (if (/= COTA_DG3 nil) (setq COTA_DG3 (rtos COTA_DG3 2 2)) (setq COTA_DG3 "") )
        (if (/= COTA_DG4 nil) (setq COTA_DG4 (rtos COTA_DG4 2 2)) (setq COTA_DG4 "") )
        (if (/= ALT_CHA nil)  (setq ALT_CHA (rtos ALT_CHA 2 2)) (setq ALT_CHA ""))
        (command "_.layer" "m" "PECAS_ESGOTO" "")
  	(princ "\nForneça a orientação da Peça.")
        (setvar "osmode" 4)
        (command "line" Pto_PV PAUSE  "")
        (setq lista (entget (entlast)))
        (setq Paux (cdr (assoc 11  lista)))
        (command "erase" "Last" "")
        (setq ANG (atof (angtos (angle Pto_PV Paux)0 12)))
        (setvar "osmode" 0)
        (setq TIPO (substr NOME_PV 1 2))
        (setq NUM_PV (substr NOME_PV 3 10))
        ;;(print TIPO)
        (cond
	  	((= tipo "PV")
        		(if (= FLAG 0)
              			(command "_.insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\PV.dwg" Pto_PV "" "" (OCTANTE ANG) "" NUM_PV CODIGO MATERPV OBSERV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) ALT_CHA (strcat "P= " (rtos PROF 2 2)))
			(command "_.insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\PVDG.dwg" Pto_PV "" "" (OCTANTE ANG) "" NUM_PV CODIGO MATERPV OBSERV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) ALT_CHA (strcat "P= " (rtos PROF 2 2)) COTA_DG1 COTA_DG2 COTA_DG3 COTA_DG4)
        		) ;;fim do if
		 )
		((= tipo "CP")
			(command "_.insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\CP.dwg" Pto_PV "" "" (OCTANTE ANG) COTA_DG4 COTA_DG3 COTA_DG2 COTA_DG1 (strcat "P= " (rtos PROF 2 2)) ALT_CHA (rtos COTA_F 2 2) (rtos COTA_T 2 2) (rtos COTA_TP 2 2) OBSERV MATERPV CODIGO NUM_PV "")
		 );;fim do cond CP
		((= tipo "CI")
		     	(progn
			        (setq TIPOCI (getint "\nCI de 40 ou 60?: "))
			        (if (= TIPOCI 40)
					(command "_.insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\CX40.dwg" Pto_PV "" "" (OCTANTE ANG) "" NUM_PV CODIGO MATERPV OBSERV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) ALT_CHA (strcat "P= " (rtos PROF 2 2)) COTA_DG1 COTA_DG2 COTA_DG3 COTA_DG4)
					(command "_.insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\CX60.dwg" Pto_PV "" "" (OCTANTE ANG) "" NUM_PV CODIGO MATERPV OBSERV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) ALT_CHA (strcat "P= " (rtos PROF 2 2)) COTA_DG1 COTA_DG2 COTA_DG3 COTA_DG4)
				  );;fim do if
			);;fim do progn
		 );; fim do cond CI
		
        );;fim do cond		
       	(command "_.layer" "m" "TXT_PECAS_ESGOTO" "")
      	(setq p1 (getpoint "Onde quer escrever os dados do PV?:"))
        (setq ANG1 (atof (angtos (angle Pto_PV p1)0 12)))
    	(cond
		    ((and (>= ANG1 0.0)(< ANG1 90.0)) (setq p3 (c:cal "p1 + [10.0,0.0,0.0]")))
		    ((and (>= ANG1 90.0)(< ANG1 270.0)) (setq p3 (c:cal "p1 - [10.0,0.0,0.0]")))
	            ((and (>= ANG1 270.0)(< ANG1 360.0)) (setq p3 (c:cal "p1 + [10.0,0.0,0.0]")))
	);;fim do cond
        (command "pline" Pto_PV p1 p3 "")
        (command "_.change" "L" "" "P" "c" 1 "")
        (command "_.change" "L" "" "P" "LW" 0.15 "")
      	(setq ANG (atof (angtos (angle Pto_PV p1)0 12)))
        (cond
		((and (>= ANG 0.0)(< ANG 90.0))
		  (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADOSD.dwg" p1 "" "" "" NOME_PV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) (rtos PROF 2 2) "") 
		 )
		((and (>= ANG 270.0)(< ANG 360.0))
		  (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADOSD.dwg" p1 "" "" "" NOME_PV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) (rtos PROF 2 2) "")
		 )
		((and (>= ANG 90.0)(< ANG 270.0))
		   (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADOSE.dwg" p1 "" "" "" NOME_PV (rtos COTA_TP 2 2) (rtos COTA_T 2 2) (rtos COTA_F 2 2) (rtos PROF 2 2) "")
		 )
	  )

 )

(defun DADOS_SETA ( DADOS1 / FLAG ANG ANG1 TamText p1 TEXTO1 DIST PTOMED ANG DECLIV Paux Pto_PV COTA_F COTA_FAux DIAM MATER)
    (setq escseta 1.0)
    (setq Paux (nth 0 DADOS1))
    (setq Pto_PV (nth 1 DADOS1))
    (setq COTA_F (nth 2 DADOS1))
    (setq COTA_FAux (nth 3 DADOS1))
    (setq DIAM (nth 4 DADOS1))
    (setq MATER (nth 5 DADOS1))
    (setq FLAG "S")
    (setq DIST (DISTANCE Paux Pto_PV))
    (if (> DIST 100.0) (setq FLAG (getString "\nDistância Superior a 100 m. Deseja Continuar? <S> ou <N>: ")))
    (setq FLAG (strcase FLAG))
    (if (= FLAG "S")
      (progn
	(command "_.layer" "m" "REDE_ESGOTO" "")
        (command "line" Paux Pto_PV "")
	(cond
		((= DIAM  50.0) (progn (setq escseta 0.5) (command "_.change" "L" "" "P" "c" 2 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  60.0) (progn (setq escseta 0.5) (command "_.change" "L" "" "P" "c" 2 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  75.0) (progn (setq escseta 0.5) (command "_.change" "L" "" "P" "c" 2 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  85.0) (progn (setq escseta 0.5) (command "_.change" "L" "" "P" "c" 2 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  100.0) (progn (setq escseta 0.5) (command "_.change" "L" "" "P" "c" 2 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  150.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  160.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.30 "")))
		((= DIAM  200.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.40 "")))
		((= DIAM  250.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.40 "")))
		((= DIAM  300.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.40 "")))
		((= DIAM  350.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.50 "")))
		((= DIAM  400.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.50 "")))
		((= DIAM  450.0) (progn (command "_.change" "L" "" "P" "c" 5 "") (command "_.change" "L" "" "P" "LW" 0.50 "")))
		((= DIAM  500.0) (progn (command "_.change" "L" "" "P" "c" 6 "") (command "_.change" "L" "" "P" "LW" 0.60 "")))
		((= DIAM  600.0) (progn (command "_.change" "L" "" "P" "c" 6 "") (command "_.change" "L" "" "P" "LW" 0.60 "")))
		((= DIAM  800.0) (progn (command "_.change" "L" "" "P" "c" 6 "") (command "_.change" "L" "" "P" "LW" 0.60 "")))
		((= DIAM  1000.0) (progn (command "_.change" "L" "" "P" "c" 6 "") (command "_.change" "L" "" "P" "LW" 0.60 "")))
		((> DIAM  1000.0) (progn (command "_.change" "L" "" "P" "c" 6 "") (command "_.change" "L" "" "P" "LW" 0.60 "")))		
	)
        (setq PTOMED (c:cal "Paux + Pto_PV"))
	(setq PTOMED (c:cal "PTOMED / 2"))
        (setq ANG (atof (angtos (angle Paux Pto_PV)0 12)))
        (setq DECLIV (rtos (abs (/ (- COTA_F COTA_FAux) DIST)) 2 4))
        (setq TEXTO1 (strcat MATER "-" (itoa DIAM) "-" DECLIV))
        (setq TamText (* (strlen TEXTO1) 1.3))
	(command "_.layer" "m" "TXT_PECAS_ESGOTO" "")
        ( if (< TamText DIST)
	     (if (< (- COTA_F COTA_FAux) 0)
	        (progn
        	     (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADO_SETA.dwg" PTOMED "" "" (OCTANTE ANG) TEXTO1 (rtos DIST 2 2) "")
		     (command "_.layer" "m" "REDE_ESGOTO" "")
		     (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\SETA1.dwg" PTOMED escseta "" ANG "")
                )
	        (progn
		     (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADO_SETA.dwg" PTOMED "" "" (OCTANTE ANG) TEXTO1 (rtos DIST 2 2) "")
		     (command "_.layer" "m" "REDE_ESGOTO" "")
		     (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\SETA1.dwg" PTOMED escseta "" (+ ANG 180.0) "")
                )
	     );;fim do if
	     (progn
	        (command "_.layer" "m" "REDE_ESGOTO" "")
        	(if (< (- COTA_F COTA_FAux) 0)
        		(command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\SETA1.dwg" PTOMED escseta "" ANG "")
	        	(command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\SETA1.dwg" PTOMED escseta "" (+ ANG 180.0) "")
	  	);;fim do if
       		(command "_.layer" "m" "TXT_PECAS_ESGOTO" "")
      		(setq p1 (getpoint "Onde quer escrever os dados da rede?:"))
        	(setq ANG1 (atof (angtos (angle Pto_PV p1)0 12)))
    			(cond
		    		((and (>= ANG1 0.0)(< ANG1 90.0)) (setq p3 (c:cal "p1 + [25.0,0.0,0.0]")))
		    		((and (>= ANG1 90.0)(< ANG1 270.0)) (setq p3 (c:cal "p1 - [25.0,0.0,0.0]")))
	            		((and (>= ANG1 270.0)(< ANG1 360.0)) (setq p3 (c:cal "p1 + [25.0,0.0,0.0]")))
			);;fim do cond
        	(command "pline" PTOMED p1 p3 "")
        	(command "_.change" "L" "" "P" "c" 1 "")
	        (command "_.change" "L" "" "P" "LW" 0.15 "")
	        (setq ANG1 (atof (angtos (angle PTOMED p1)0 12)))
	        (cond
		    ((and (>= ANG1 0.0)(< ANG1 90.0))
                       (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADO_SETA_D.dwg" P1 "" "" "" TEXTO1 (rtos DIST 2 2) ""))
		    ((and (>= ANG1 270.0)(< ANG1 360.0))
                       (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADO_SETA_D.dwg" P1 "" "" "" TEXTO1 (rtos DIST 2 2) ""))
		    ((and (>= ANG1 90.0)(< ANG1 270.0))
	                (command "insert" "F:\\Biblioteca CAD\\Blocos\\CAESB_NOVO\\DADO_SETA_E.dwg" p1 "" "" "" TEXTO1 (rtos DIST 2 2) ""))	
		);;fim do cond
	     );;fim do progn  
	);;fim fo if
	))
)

(defun PARTE1 ( / )
    (setq Pto_PV nil NOME_PV nil COTA_TP nil COTA_T nil COTA_F nil PROF nil COTA_DG1 nil COTA_DG2 nil COTA_DG3 nil COTA_DG4 nil ALT_CHA nil OBSERV nil MATERPV  nil)
    (setq Conjunto (ssget '((0 . "INSERT"))))
    (setq Comp_Conj (sslength Conjunto))
    (if (>= (sslength Conjunto) 3)
     	  (progn 
		(setq Comp_Conj (sslength Conjunto))
		(while (> Comp_Conj 0) ;; primeiro while
			(progn
	      			(setq Comp_Conj (1- Comp_Conj))
	      			(setq OBJETO (ssname Conjunto Comp_Conj))
              			(setq DADOS (DADOS_BLOCO OBJETO))
              			(setq DESC (nth 1 DADOS))
	      			(cond
                   			((= DESC "F") (setq COTA_F (atof(nth 2 DADOS))))
                   			((= DESC "C") (setq COTA_T (atof(nth 2 DADOS))))
                                        ((= DESC "DG1") (setq COTA_DG1 (atof(nth 2 DADOS))))
                                        ((= DESC "DG2") (setq COTA_DG2 (atof(nth 2 DADOS))))
                                        ((= DESC "DG3") (setq COTA_DG3 (atof(nth 2 DADOS))))
                                        ((= DESC "DG4") (setq COTA_DG4 (atof(nth 2 DADOS))))
                                        ((= DESC "PES") (setq COTA_PES (atof(nth 2 DADOS))))
                                        (T otherwise 
                       			   (progn 
			   			(setq Pto_PV (cdr (assoc 10 (entget OBJETO))))
        					(setq NOME_PV (getstring "\nForneça o número do PV, CP ou CI: "))
        					(setq NOME_PV (strcase NOME_PV))
                           			;;(setq NOME_PV (nth 1 DADOS))
                           			(setq COTA_TP (setq COTA_TP (atof(nth 2 DADOS))))
                       			   );;fim do progn
                  		 	);;fim do otherwise
                
              			);;fim do cond
			    );;fim do progn 
         	);;fim while 
         	(setq PROF (- COTA_T COTA_F))
                (if (/= COTA_PES nil) (setq ALT_CHA  (- COTA_TP COTA_PES)))
	        (if (/= COTA_DG1 nil) (setq COTA_DG1 (- COTA_DG1 COTA_F)))
	    	(if (/= COTA_DG2 nil) (setq COTA_DG2 (- COTA_DG2 COTA_F)))
	    	(if (/= COTA_DG3 nil) (setq COTA_DG3 (- COTA_DG3 COTA_F)))
	    	(if (/= COTA_DG4 nil) (setq COTA_DG4 (- COTA_DG4 COTA_F)))
                (setq MATERPV (getstring "\nForneça o Material da Peça <PVC> <CA> <MBV>: "))
                (setq MATERPV (strcase MATERPV))
                (setq OBSERV (getstring "\nDescreva a observação da Peça: "))
                (setq OBSERV (strcase OBSERV))
		(setq PV (list 	Pto_PV NOME_PV COTA_TP COTA_T COTA_F PROF COTA_DG1 COTA_DG2 COTA_DG3 COTA_DG4 ALT_CHA CODIGO OBSERV MATERPV))
		(DADOS_PV)
	  );;fim do progn
          (progn
	  	(princ "\nO tamanho da seleção está errado. Tente de novo.")
	        (PARTE1)
	  )
       )
    )
(defun PARTE2 ( / )

   ;;(princ Conjunto)

   (while T
        (princ "\nDiâmetros mm: <50>;<60>;<75>;<85>;<100>")
        (princ "\nDiâmetros mm: <150>;<160>;<200>;<250>;<300>;<350>;<400>")
        (princ "\nDiâmetros mm: <450>;<500>;<600>;<800>;<1000>;<OUTROS>")
        (initget 7)
        (setq DIAM (getint "\nForneça o diâmetro da rede: "))
        (princ "\nMaterias: <PVC>;<FF>;<AÇO>;<CA>;<MBV>")
        (setq MATER (getstring "\nForneça o material da rede: "))
        (setq MATER (strcase MATER))
        (princ MSN1)
        (setq Paux Pto_PV)
        (setq COTA_FAux COTA_F)
   	(PARTE1)
        (setq LISTA_SETA (list Paux Pto_PV COTA_F COTA_FAux DIAM MATER))
        (DADOS_SETA LISTA_SETA)
     );;fim do while  		
)



(defun c:REDE ( / Conjunto LISTA_SETA TEXTO1 OBSERV CODIGO MATER MATERPV DIAM Comp_Conj Pto_PV CPD DESC COTA_PES COTA_DG1 COTA_DG2 COTA_DG3 COTA_DG4 COTA_F COTA_T COTA_TP NOME_PV ALT_CHA PROF p1 Paux PROF_Aux MSN1 DIST PTOMED DECLIV )
   (C:cal "1+1")
   (setq oldangbase (getvar "angbase"));;correção 2
   (setvar "angbase" 0.0)
   (setq oldangdir (getvar "angdir"));;correção 2
   (setvar "angdir" 0)
   (setq oldosmode (getvar "osmode"))
   (setvar "osmode" 0)
   (setq oldcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (command "_.layer" "m" "PECAS_ESGOTO" "")
   (command "_.layer" "m" "TXT_PECAS_ESGOTO" "")
   (setq CODIGO (getstring "\nForneça o código da obra: "))
   (setq CODIGO (strcase CODIGO))
   (setq MSN1 "Selecione os pontos:")
   (princ MSN1)
   (PARTE1)
   (PARTE2)
   ;;(princ Conjunto)
   (setvar "osmode" oldosmode)
   (setvar "cmdecho" olcmdecho)
)

(princ "\nDigite 'REDE' para iniciar")