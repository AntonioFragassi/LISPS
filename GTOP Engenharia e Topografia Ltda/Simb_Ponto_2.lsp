;; Coloca simbolo de pontos com numeração sequencial

(defun DADOS_BLOCO ( OBJETO / DADOS1 lista Attn Attv)
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

(defun c:bota_simb ( / olcmdecho escala ponto i PONTO DESC COTA)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
	(setq escala (getreal "Escala do desenho: "))
        (setq escala (/ escala 1000.0))
	(while (setq bloco (car (entsel "Selecione o Bloco: " ))) ;; primeiro while  
	        (DADOS_BLOCO bloco)
	        (setq Pto_bloco (cdr (assoc 10 (entget bloco))))
	        (command "_.layer" "m" "PONTOS" "")
      		(command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\SIMB_PONTO_1.dwg" Pto_bloco escala "" "0" DESC)
	) ;;fim do primeiro if
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'Bota_simb' para iniciar")