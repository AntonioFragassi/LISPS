;; Coloca arvore irradiamentos segundo a descrição ARV

(defun c:arv( / olcmdecho oldlayer escala Conjunto_INSERT Comp_Conjunto Objeto ponto lista  Attn Attv diam)
(princ "********* Espere um pouco - processando dados  ***********")
(setq olcmdecho (getvar "cmdecho"))
(setvar "cmdecho" 0) 
(setq oldlayer (getvar "clayer"))
(setq escala (getreal "Escala do desenho: "))
(setq escala (/ escala 1000.0))
(SETQ OPCAO (GETINT "\nComo deseja escrever? D = 100 mm <1>,  100: <2>  ou Nada <3>"))
(setq Conjunto_INSERT (ssget '((0 . "INSERT"))))
(if Conjunto_INSERT ;; primeiro if
  (COND
        ( (= OPCAO 1)
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
					
                                                                
                                                          
                                                                (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))                                                                
                                                                (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))         
                                                              



					);;fim do cond         
		    );;fim do progn
                          );;fim do if
	      );;fim do while
             );fim do while
          ));fim do progn e fim da primeira opção

        ( (= OPCAO 2)
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
				
                                                                (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))
                                                                



                                   );;fim do cond
		          );;fim do progn
                      );;fim do if
	      );;fim do while
             );fim do while
          ));fim do progn e fim da segunda opção

        ( (= OPCAO 3)
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
		                   (setq desc "")
		        	   (setq Attv (substr Attv 1 3))
                                   (cond
				
                                                                (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "TERRANOVA_coqpalm.dwg" ponto escala "" "0" desc)))
                                                               




                                   );;fim do cond
		          );;fim do progn
                      );;fim do if
	      );;fim do while
             );fim do while
          ));fim do progn e fim da segunda opção

      );fim do cond

) ;;fim do primeiro if
(setvar "cmdecho" olcmdecho)
(setvar "clayer" oldlayer)
(princ)
)
(princ "\nDigite 'arv' para iniciar")