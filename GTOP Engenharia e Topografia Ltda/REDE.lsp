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
  	(setq COTA_T (nth 1 DADOS1))
  	(setq COTA_F (nth 2 DADOS1))
      	(command "_.layer" "m" "TEXTO" "")
      	(command "line" Pto_PV (setq p1 (getpoint "Onde quer escrever os dados?:")) "")
        (command "_.change" "L" "" "P" "c" 7 "")
      	(setq ANG (atof (angtos (angle Pto_PV p1)0 12)))
        (cond
		((and (>= ANG 0.0)(< ANG 90.0))
		  (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\REDED.dwg" p1 "" "" "" (strcat "REDE=" (rtos COTA_F 2 3)) (strcat "TERRENO="(rtos COTA_T 2 3)) "") 
		 )
		((and (>= ANG 270.0)(< ANG 360.0))
		  (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\REDED.dwg" p1 "" "" "" (strcat "REDE=" (rtos COTA_F 2 3)) (strcat "TERRENO="(rtos COTA_T 2 3)) "")
		 )
		((and (>= ANG 90.0)(< ANG 270.0))
		   (command "insert" "T:\\Biblioteca CAD\\Blocos\\CAESB\\REDEE.dwg" p1 "" "" "" (strcat "REDE=" (rtos COTA_F 2 3)) (strcat "TERRENO="(rtos COTA_T 2 3)) "")
		 )
	  )

 )


(defun PARTE1 ( / )
    (setq Conjunto (ssget '((0 . "INSERT"))))
    (setq Comp_Conj (sslength Conjunto))
    (if (= (sslength Conjunto) 2)
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
                   			((= DESC "C") (setq COTA_T (atof(nth 2 DADOS))))
                   			((= DESC "RD") 
                       			   (progn 
			   			(setq Pto_PV (cdr (assoc 10 (entget OBJETO))))
                           			(setq COTA_F (setq COTA_F (atof(nth 2 DADOS))))
                       			   );;fim do progn
                  		 	);;fim do otherwise
              			);;fim do cond
			    );;fim do progn 
         	);;fim while 
		(setq PV (list 	Pto_PV COTA_T COTA_F))
		(DADOS_PV PV)
	  );;fim do progn
          (progn
	  	(princ "\nO tamanho da seleção está errado. Tente de novo.")
	        (PARTE1)
	  )
       )
    )



(defun c:REDE ( / Conjunto LISTA_SETA TEXTO1 MATER DIAM Comp_Conj Pto_PV CPD DESC COTA_F COTA_T COTA_TP NOME_PV PROF p1 Paux PROF_Aux MSN1 DIST PTOMED DECLIV )
   (C:cal "1+1")
   (setq oldosmode (getvar "osmode"))	
   (setvar "osmode" 0)
   (setq olcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (command "_.layer" "m" "TEXTO" "")
   (setq MSN1 "Selecione os pontos:")
   (princ MSN1)
   (PARTE1)
   ;;(princ Conjunto)
   (setvar "osmode" oldosmode)
   (setvar "cmdecho" olcmdecho)
)

(princ "\nDigite 'REDE' para iniciar")