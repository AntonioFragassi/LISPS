;; Coloca numero do lote com numeração sequencial
(defun c:NTEXTO ( /)
        (setq i (getint "Número inicial: "))
	(while (setq Pto_bloco (Getpoint "Selecione o Ponto: " )) ;; primeiro while 
                (if (< i 0)
                  (setq DESC (strcat "" (itoa i)))
                  (setq DESC (itoa i))
                ) 
      		(command "_.TEXT" "j" "mc" Pto_bloco "500000000" "0" DESC)
                (setq i (1+ i))
	) ;;fim do primeiro if
	(princ)
)
(princ "\nDigite 'NTEXTO' para iniciar")