;; Coloca arvore irradiamentos segundo a descrição ARV

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
	 ))  
        (setq DADOS1 (append DADOS1 (list PONTO) (list DESC) (list COTA)))
        (setq DADOS1 DADOS1)
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
                                (setq AUXDESC (substr DESC 1 3))
	      			(cond
                   			         ((= AUXDESC "COP") (setq COTA_COPA (atof(nth 2 DADOS))))
                   			         ((= AUXDESC "ARV") 
                       			             (progn 
			   					(setq Pto_ARV (cdr (assoc 10 (entget OBJETO))))
                           			            	(setq NOME_ARV (nth 1 DADOS))
                           			            	(setq COTA_ARV (setq COTA_ARV (atof(nth 2 DADOS))))
                       			             ));;fim do progn
						 ((= AUXDESC "COQ") 
                       			             (progn 
			   					(setq Pto_ARV (cdr (assoc 10 (entget OBJETO))))
                           			            	(setq NOME_ARV (nth 1 DADOS))
                           			            	(setq COTA_ARV (setq COTA_ARV (atof(nth 2 DADOS))))
                       			             ));;fim do progn
              			);;fim do cond
			    );;fim do progn 
         	);;fim while 
         	(setq ALT_ARV (strcat "h = " (rtos (- COTA_COPA COTA_ARV) 2 3) "m"))
		(setq ARV (list Pto_ARV NOME_ARV COTA_ARV COTA_ARV COTA_COPA ALT_ARV))
		(DADOS_ARV)
	  );;fim do progn
          (progn
	  	(princ "\nO tamanho da seleção está errado. Tente de novo.")
	        (PARTE1)
	  )
       )
    )

(defun DADOS_ARV ( / Pto_ARV NOME_ARV COTA_ARV COTA_ARV COTA_TOPO ALT_ARV p1 Attn Attv ) ;; ARV (Pto_ARV NOME_ARV COTA_ARV COTA_ARV COTA_TOPO ALT_ARV))
      	     (setq Ponto (nth 0 ARV))
      	     (setq NOME_ARV (nth 1 ARV))
  	     (setq COTA_ARV (nth 2 ARV))
  	     (setq COTA_ARV (nth 3 ARV))
  	     (setq COTA_COPA (nth 4 ARV))
  	     (setq ALT_ARV (nth 5 ARV))
             (setq desc (substr NOME_ARV 4 ))
             (setq Attv (substr NOME_ARV 1 3))
             (cond
		          (( = Attv "ARV" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "coqpalm1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "coqpalm1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "GOI" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "BAN" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "ABC" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "LIM" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "MAN" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "MAO" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
                                        (( = Attv "AMR" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))
	                                (( = Attv "EUC" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore1.dwg" ponto escala "" "0" desc ALT_ARV)))

               );;fim do cond


 )


(defun c:arv1 ( / olcmdecho oldlayer escala ARV)
(princ "********* Espere um pouco - processando dados  ***********")
(setq olcmdecho (getvar "cmdecho"))
(setvar "cmdecho" 0) 
(setq oldlayer (getvar "clayer"))
(setq escala (getreal "Escala do desenho: "))
(setq escala (/ escala 1000.0))
(while T
   (PARTE1)
)
(setvar "cmdecho" olcmdecho)
(setvar "clayer" oldlayer)
(princ)
)
(princ "\nDigite 'arv1' para iniciar")