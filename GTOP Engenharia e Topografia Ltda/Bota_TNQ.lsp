;; Coloca os blocos segundo a descrição 

(defun c:tnq( / olcmdecho oldlayer escala Conjunto_INSERT Comp_Conjunto Objeto ponto lista  Attn Attv diam)
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
					
                                        (( = Attv "TNQ" ) (progn (command "_.layer" "m" "TANQUE" "")(command "_.insert" "tanque.dwg" ponto escala "" "0" "")))                                        
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
(princ "\nDigite 'tnq' para iniciar")