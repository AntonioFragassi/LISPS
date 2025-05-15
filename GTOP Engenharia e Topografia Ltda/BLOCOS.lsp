;; Coloca os blocos segundo a descrição 

(defun c:blk( / olcmdecho oldlayer escala Conjunto_INSERT Comp_Conjunto Objeto ponto lista  Attn Attv diam)
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
					(( = Attv "LUM" ) (progn (command "_.layer" "m" "LUMINARIA" "")(command "_.insert" "luminaria.dwg" ponto escala "" "0" "") ))
                                        (( = Attv "MAC" ) (progn (command "_.layer" "m" "MARCO_CONCRETO" "")(command "_.insert" "SIMB_MARCO.dwg" ponto escala "" "0" "") ))
                                        (( = Attv "MOR" ) (progn (command "_.layer" "m" "MOURAO_MADEIRA" "")(command "_.insert" "morao_madeira.dwg" ponto escala "" "0" "") ))
			   (( = Attv "PAP" ) (progn (command "_.layer" "m" "PV_DE_AGUA_PLUVIAL" "")(command "_.insert" "pv_aguapluv.dwg" ponto escala "" "0" "")))
                                         (( = Attv "PVA" ) (progn (command "_.layer" "m" "PV_DE_AGUA_PLUVIAL" "")(command "_.insert" "pv_aguapluv.dwg" ponto escala "" "0" "")))
			   (( = Attv "POC" ) (progn (command "_.layer" "m" "POSTE_CONCRETO" "")(command "_.insert" "poste_concreto.dwg" ponto escala "" "0" "")))
			   (( = Attv "PTR" ) (progn (command "_.layer" "m" "POSTE_CONCRETO" "")(command "_.insert" "poste_concreto.dwg" ponto escala "" "0" "PC-T")))
                                         (( = Attv "PCT" ) (progn (command "_.layer" "m" "POSTE_CONCRETO" "")(command "_.insert" "poste_concreto.dwg" ponto escala "" "0" "PC-T")))
		               (( = Attv "PVN" ) (progn (command "_.layer" "m" "PV_NAO_IDENTIFICADO" "")(command "_.insert" "pv_naoidentificado.dwg" ponto escala "" "0" "")))
			  (( = Attv "PES" ) (progn (command "_.layer" "m" "PV_ESGOTO" "")(command "_.insert" "pv_esgoto.dwg" ponto escala "" "0" "") ))
		               (( = Attv "TUP" ) (progn (command "_.layer" "m" "TELEFONE_PUBLICO" "")(command "_.insert" "Tel_publico.dwg" ponto escala "" "0" "")))
		               (( = Attv "PTL" ) (progn (command "_.layer" "m" "PV_TELEFONE" "")(command "_.insert" "pv_telefone.dwg" ponto escala "" "0" "")))
			  (( = Attv "SIV" ) (progn (command "_.layer" "m" "SENSOR_INFRAVERMELHO" "")(command "_.insert" "sensor_infravermelho.dwg" ponto escala "" "0" "")))
                                        (( = Attv "PQT" ) (progn (command "_.layer" "m" "PIQUETE" "")(command "_.insert" "piquete.dwg" ponto escala "" "0" "")))
                                        (( = Attv "ETC" ) (progn (command "_.layer" "m" "ESTACA" "")(command "_.insert" "estaca.dwg" ponto escala "" "0" "")))
                                        (( = Attv "PIL" ) (progn (command "_.layer" "m" "PILAR" "")(command "_.insert" "pilar.dwg" ponto escala "" "0" "")))
                                        (( = Attv "REF" ) (progn (command "_.layer" "m" "REFLETOR" "")(command "_.insert" "Refletor.dwg" ponto escala "" "0" "")))
                                        (( = Attv "REG" ) (progn (command "_.layer" "m" "REGISTRO_AGUA" "")(command "_.insert" "registro_agua.dwg" ponto escala "" "0" "")))
                                        (( = Attv "PVT" ) (progn (command "_.layer" "m" "PV_TELEFONE" "")(command "_.insert" "pv_telefone.dwg" ponto escala "" "0" "")))
                                        (( = Attv "PEL" ) (progn (command "_.layer" "m" "PV_ELETRICIDADE" "")(command "_.insert" "pv_eletricidade.dwg" ponto escala "" "0" "")))
                                        (( = Attv "CPF" ) (progn (command "_.layer" "m" "CAIXA_PASSAGEM_FIOS" "")(command "_.insert" "cx_pass_fio.dwg" ponto escala "" "0" "")))
                                        (( = Attv "SMF" ) (progn (command "_.layer" "m" "SEMAFORO" "")(command "_.insert" "Semaforo.dwg" ponto escala "" "0" "")))
                                        (( = Attv "PVN" ) (progn (command "_.layer" "m" "PV_NAO_IDENTIFICADO" "")(command "_.insert" "pv_naoidentificado.dwg" ponto escala "" "0" "")))
                                        (( = Attv "RLE" ) (progn (command "_.layer" "m" "RELOGIO_ELETRONICO" "")(command "_.insert" "relogio_eletronico.dwg" ponto escala "" "0" "")))
                                        
                                        
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
(princ "\nDigite 'blk' para iniciar")