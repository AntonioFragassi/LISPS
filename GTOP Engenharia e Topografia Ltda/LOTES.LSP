;; Coloca pontos em lote com atributos: quadra, conjunto, lote e pto
(defun c:lotes ( / olcmdecho escala ponto i)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
        (setq escala 1)
        ;(setq CONJUNTO (getstring "Fonerça o número do Conjunto: "))
        (command "_.layer" "m" "PONTO_LOTE" "")
        (while T
	    (setq QUADRA (getstring "Fonerça o número da Quadra: "))
            (setq LOTE (getint "\nFonerça o número incial da contagem do lote: "))
	    (while (setq ponto ( getpoint "\nClique em um ponto: ")) ;; primeiro while	
      		(command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\LOTE_1.dwg" ponto escala "" "0" QUADRA "" LOTE)
                (setq LOTE (1+ LOTE))
	    ) ;;fim do while
        );; FIM DO WHILE
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'lotes' para iniciar")