;; Coloca o bloco de ponto cotado 

(defun c:pto( / olcmdecho oldlayer escala Conjunto_INSERT Comp_Conjunto Objeto ponto lista  Attn Attv diam Cota)
(princ "********* Espere um pouco - processando dados  ***********")
(setq olcmdecho (getvar "cmdecho"))
(setvar "cmdecho" 0) 
(setq oldlayer (getvar "clayer"))
(setq escala (getreal "Escala do desenho: "))
(setq escala (/ escala 1000.0))
(setq Conjunto_INSERT (ssget '((0 . "INSERT"))))
(if Conjunto_INSERT ;; primeiro if
     (progn 
	(setq Comp_Conjunto (sslength Conjunto_INSERT))
	(while (> Comp_Conjunto 0) ;; primeiro while
	      (setq Comp_Conjunto (1- Comp_Conjunto))
	      (setq Objeto (ssname Conjunto_INSERT  Comp_Conjunto))
              (setq ponto (cdr (assoc 10 (entget Objeto))))
              (setq cota (rtos (last ponto) 2 3))
 	      (while (/= (cdr (assoc 0 (entget OBJETO))) "SEQEND")
        	(setq OBJETO (entnext OBJETO))
  		(setq lista (entget OBJETO))
       		(setq Attn (cdr (assoc 2 lista)))
  		(setq Attv (cdr (assoc 1 lista)))
        		(if (= Attn "DESC")
	                  (progn
		                (setq desc (substr Attv 4 ))
		        	   (setq Attv (substr Attv 1 3))
                                         (cond
					(( = Attv "C" ) (progn (command "_.layer" "m" "COTA_APARELHO" "")(command "_.insert" "cota_de_aparelho.dwg" ponto escala "" "0" cota) ))
					(( = Attv "CCC" ) (progn (command "_.layer" "m" "COTA_APARELHO" "")(command "_.insert" "cota_de_aparelho.dwg" ponto escala "" "0" cota)))
                                    );;fim do cond
		    );;fim do progn
                          );;fim do if
	      );;fim do while
             );fim do while
      );fim do progn
) ;;fim do primeiro if
(setvar "cmdecho" olcmdecho)
(setvar "clayer" oldlayer)
(princ)
)
(princ "\nDigite 'pto' para iniciar")