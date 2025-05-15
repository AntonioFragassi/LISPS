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
					
                                                                
                                                                (( = Attv "GOI" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PIN" ) (progn (command "_.layer" "m" "PINHEIRO" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "pinheiro.dwg" ponto escala "" "0" desc)))
								(( = Attv "JBO" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GOB" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PIT" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "ABC" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAN" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "AMO" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "EUC" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "CJU" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAM" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "LOB" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAG" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PEQ" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PIN" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAC" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAQ" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "ACE" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAT" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "CFE" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "CJU" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "LAR" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "TAG" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
						 		(( = Attv "MIX" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "AME" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAO" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc))) 
                                                                (( = Attv "LIM" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))         
                                                                (( = Attv "BAN" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc))) 
                                                                (( = Attv "ARV" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc))) 
                                                                (( = Attv "MAM" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                            					(( = Attv "BUR" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAB" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "ACE")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
 								(( = Attv "SIR")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CFE")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GRA")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PTG")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "TAM")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CRB")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJM")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CNE")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMX")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" "")))
								(( = Attv "CJA")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" "")))
								(( = Attv "IPE")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" "")))
								(( = Attv "CAR")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" "")))
								(( = Attv "BUR")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" "")))
								(( = Attv "AMD")(progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
 								(( = Attv "AMR" ) (progn (command "_.layer" "m" "ARVORE" "")(setq desc (strcat "%%C=" desc "mm"))(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
	



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
								(( = Attv "ARV" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PIN" ) (progn (command "_.layer" "m" "PINHEIRO" "")(command "_.insert" "pinheiro.dwg" ponto escala "" "0" desc)))
								(( = Attv "JBO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "GOI" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GOB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PIT" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "BAN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "ABC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "LIM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "AMR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "EUC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "CAJ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "LOB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "MAG" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PEQ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "PIN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                                (( = Attv "JAC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))   
                                                                (( = Attv "ACE" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJU" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "FRB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "LRJ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PBR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PEQ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "ROM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "TAG" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "MIX" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                        	           	(( = Attv "ARB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                        	           	(( = Attv "AMO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                        	           	(( = Attv "MAM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                  	      	           	(( = Attv "JAB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "ACE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "SIR")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CFE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PTG")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GRA")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "TAM")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CRB")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJM")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CNE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "IPE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMX")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJA")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "antena.dwg" ponto escala "" "0" "")))
								(( = Attv "CAR")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "antena.dwg" ponto escala "" "0" "")))
								(( = Attv "BUR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMD" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))








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
								(( = Attv "ARV" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "JBO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "COQ" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "PAL" ) (progn (command "_.layer" "m" "COQUEIRO_PALMEIRA" "")(command "_.insert" "coqpalm.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "GOI" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GOB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "BAN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PIT" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "ABC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "LIM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "MAN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "MAO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "AMO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "EUC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "CAJ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "JAM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "LOB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "MAG" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "PIQ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "PIN" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                                                              (( = Attv "JAC" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))  
                                                              (( = Attv "ACE" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PIN" ) (progn (command "_.layer" "m" "PINHEIRO" "")(command "_.insert" "pinheiro.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJU" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "MIX" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "FRB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "LRJ" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PBR" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PQI" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "ROM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "TAG" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                               		       		   	(( = Attv "ARB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                               		     		    	(( = Attv "AMO" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                               		 		     (( = Attv "MAM" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
                               		    		      (( = Attv "JAB" ) (progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "ACE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "SIR")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CFE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "GRA")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "PTG")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "TAM")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CRB")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJM")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CNE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMX")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CJA")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "IPE")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "CAR")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "BUR")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
								(( = Attv "AMD")(progn (command "_.layer" "m" "ARVORE" "")(command "_.insert" "arvore.dwg" ponto escala "" "0" desc)))
	









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