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

(defun DADOS_PV ( DADOS1 / Pto_PV NOME_PV COTA_TP COTA_T COTA_F PROF p1) ;; PV (list Pto_PV NOME_PV COTA_TP COTA_T COTA_F PROF))
      	(setq Pto_PV (nth 0 DADOS1))
      	(setq NOME_PV (nth 1 DADOS1))
  	(setq COTA_TP (nth 2 DADOS1))
  	(setq COTA_T (nth 3 DADOS1))
  	(setq COTA_F (nth 4 DADOS1))
  	(setq PROF (nth 5 DADOS1))
        (command "_.layer" "m" "REDE_ESGOTO" "")
        (command "insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\pv_esgoto.dwg" Pto_PV fator "" "" "") 
      	(command "_.layer" "m" "TEXTOS" "")
      	(command "line" Pto_PV (setq p1 (getpoint "Onde quer escrever os dados do PV?:")) "")
        (command "_.change" "L" "" "P" "c" 24 "")
      	(setq ANG (atof (angtos (angle Pto_PV p1)0 12)))
        (cond
		((and (>= ANG 0.0)(< ANG 90.0))
		  (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADOSD.dwg" p1 fator "" "" NOME_PV (rtos COTA_TP 2 3) (rtos COTA_T 2 3) (rtos COTA_F 2 3) (rtos PROF 2 2) "") 
		 )
		((and (>= ANG 270.0)(< ANG 360.0))
		  (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADOSD.dwg" p1 fator "" "" NOME_PV (rtos COTA_TP 2 3) (rtos COTA_T 2 3) (rtos COTA_F 2 3) (rtos PROF 2 2) "")
		 )
		((and (>= ANG 90.0)(< ANG 270.0))
		   (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADOSE.dwg" p1 fator "" "" NOME_PV (rtos COTA_TP 2 3) (rtos COTA_T 2 3) (rtos COTA_F 2 3) (rtos PROF 2 2) "")
		 )
	  )

 )

(defun DADOS_SETA ( DADOS1 / FLAG ANG ANG1 TamText p1 TEXTO1 DIST PTOMED ANG DECLIV Paux Pto_PV COTA_F COTA_FAux DIAM MATER)
    (setq Paux (nth 0 DADOS1))
    (setq Pto_PV (nth 1 DADOS1))
    (setq COTA_F (nth 2 DADOS1))
    (setq COTA_FAux (nth 3 DADOS1))
    (setq DIAM (nth 4 DADOS1))
    (setq MATER (nth 5 DADOS1))
    (command "_.layer" "m" "REDE_ESGOTO" "")
    (command "line" Paux Pto_PV "")
    (command "_.change" "L" "" "P" "c" 240 "")
    (setq FLAG "S")
    (setq DIST (DISTANCE Paux Pto_PV))
    (if (> DIST 100.0) (setq FLAG (getString "\nDistância Superior a 100 m. Deseja Continuar? <S> ou <N>: ")))
    (setq FLAG (strcase FLAG))
    (if (= FLAG "S")
      (progn
        (setq PTOMED (c:cal "Paux + Pto_PV"))
	(setq PTOMED (c:cal "PTOMED / 2"))
        (setq ANG (atof (angtos (angle Paux Pto_PV)0 12)))
        (setq DECLIV (rtos (abs (/ (- COTA_F COTA_FAux) DIST)) 2 4))
        (setq TEXTO1 (strcat MATER "-" (itoa DIAM) "-" DECLIV))
        (setq TamText (* (strlen TEXTO1) 1.3))
        ( if (< TamText DIST)
	     (if (< (- COTA_F COTA_FAux) 0)
	        (progn
        	     (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADO_SETA.dwg" PTOMED fator "" (OCTANTE ANG) TEXTO1 (rtos DIST 2 2) "")
		     (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\SETA1.dwg" PTOMED fator "" ANG "")
                )
	        (progn
		     (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADO_SETA.dwg" PTOMED fator "" (OCTANTE ANG) TEXTO1 (rtos DIST 2 2) "")
		     (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\SETA1.dwg" PTOMED fator "" (+ ANG 180.0) "")
                )
	     );;fim do if
	     (progn
        	(if (< (- COTA_F COTA_FAux) 0)
        		(command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\SETA1.dwg" PTOMED fator "" ANG "")
	        	(command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\SETA1.dwg" PTOMED fator "" (+ ANG 180.0) "")
	  	);;fim do if
      		(command "_.layer" "m" "TEXTOS" "")
      		(command "line" PTOMED (setq p1 (getpoint "Onde quer escrever os dados da REDE?:")) "")
        	(command "_.change" "L" "" "P" "c" 24 "")
	        (setq ANG1 (atof (angtos (angle PTOMED p1)0 12)))
	        (cond
		    ((and (>= ANG1 0.0)(< ANG1 90.0))
                       (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADO_SETA_D.dwg" P1 fator "" "" TEXTO1 (rtos DIST 2 2) ""))
		    ((and (>= ANG1 270.0)(< ANG1 360.0))
                       (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADO_SETA_D.dwg" P1 fator "" "" TEXTO1 (rtos DIST 2 2) ""))
		    ((and (>= ANG1 90.0)(< ANG1 270.0))
	                (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\DADO_SETA_E.dwg" p1 fator "" "" TEXTO1 (rtos DIST 2 2) ""))	
		);;fim do cond
	     );;fim do progn  
	);;fim fo if
	))
)

(defun PARTE1 ( / )
    (setq Conjunto (ssget '((0 . "INSERT"))))
    (setq Comp_Conj (sslength Conjunto))
    (if (= (sslength Conjunto) 3)
     	  (progn 
		(setq Comp_Conj (sslength Conjunto))
		;;(princ Comp_Conj)
		(while (> Comp_Conj 0) ;; primeiro while
			(progn
	      			(setq Comp_Conj (1- Comp_Conj))
	      			(setq OBJETO (ssname Conjunto Comp_Conj))
              			(setq DADOS (DADOS_BLOCO OBJETO))
              			(setq DESC (nth 1 DADOS))
	      			(cond
                   			((= DESC "F") (setq COTA_F (atof(nth 2 DADOS))))
                   			((= DESC "C") (setq COTA_T (atof(nth 2 DADOS))))
                   			(T otherwise 
                       			   (progn 
			   			(setq Pto_PV (cdr (assoc 10 (entget OBJETO))))
                           			(setq NOME_PV (nth 1 DADOS))
                           			(setq COTA_TP (setq COTA_TP (atof(nth 2 DADOS))))
                       			   );;fim do progn
                  		 	);;fim do otherwise
              			);;fim do cond
			    );;fim do progn 
         	);;fim while 
         	(setq PROF (- COTA_T COTA_F))
		(setq PV (list 	Pto_PV NOME_PV COTA_TP COTA_T COTA_F PROF))
		(DADOS_PV PV)
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
        (princ "\nDiâmetros mm: <50>;<60>;<75>;<85>;<100>;<110>;<125>")
        (princ "\nDiâmetros mm: <150>;<160>;<200>;<250>;<300>;<350>;<400>")
        (princ "\nDiâmetros mm: <450>;<500>;<600>;<800>;<1000>;<OUTROS>")
        (initget 7)
        (setq DIAM (getint "\nForneça o diâmetro da rede: "))
        (princ "\nMaterias: <PVC>;<FF>;<AÇO>;<C.A.>;<M.B.V.>")
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



(defun c:REDESG ( / Conjunto LISTA_SETA TEXTO1 MATER DIAM Comp_Conj Pto_PV CPD DESC COTA_F COTA_T COTA_TP NOME_PV PROF p1 Paux PROF_Aux MSN1 DIST PTOMED DECLIV escala fator)
   (C:cal "1+1")
   (SETQ escala (getreal "Escala do desenho: "))
   (setq fator (/ escala 1000.0))
   (setq oldosmode (getvar "osmode"))	
   (setvar "osmode" 0)
   (setq olcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (command "_.layer" "m" "REDE_ESGOTO" "")
   (command "_.layer" "m" "TEXTOS" "")
   (command "_.layer" "m" "LINHA_CHAMADA" "")
   (setq MSN1 "Selecione os pontos:")
   (princ MSN1)
   (PARTE1)
   (PARTE2)
   ;;(princ Conjunto)
   (setvar "osmode" oldosmode)
   (setvar "cmdecho" olcmdecho)
)

(princ "\nDigite 'REDESG' para iniciar")