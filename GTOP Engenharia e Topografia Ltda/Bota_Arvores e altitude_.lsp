;; Coloca o simbolo de arvore com altitude da copo apartir da descição TOPO

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
    (setq Comp_Conj (sslength Conjunto))
    (while (> Comp_Conj 0) ;; primeiro while
			(progn
	      			(setq Comp_Conj (1- Comp_Conj))
	      			(setq OBJETO (ssname Conjunto Comp_Conj))
              			(setq DADOS (DADOS_BLOCO OBJETO))
              			(setq DESC (nth 1 DADOS))
                                (cond
                    			         ((= DESC "TOPO") 
                       			             (progn 
                           			          (setq COTA_ARV (nth 2 DADOS))  	
                                                          (command "_.layer" "m" "ARVORE" "")
                                                          (command "_.insert" "arvore.dwg" ponto escala "" "0" COTA_ARV)
                       			             );;fim do progn

              			);;fim do cond
			    );;fim do progn 
     );;fim while 
)

(defun c:ARVTOPO ( / olcmdecho oldlayer escala ARV)
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
(princ "\nDigite 'ARVTOPO' para iniciar")