;; Coloca pontos em lote com atributos: quadra, conjunto, lote e pto
(defun c:ponto_lote ( / olcmdecho escala ponto i)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
        (setq escala 1)
        (setq QUADRA (getstring "Foner�a o n�mero da Quadra: "))
        (setq CONJUNTO (getstring "Foner�a o n�mero do Conjunto: "))
        (command "_.layer" "m" "PONTO_LOTE" "")
        (while T
            (setq i 1)
            (setq LOTE (getstring "Foner�a o n�mero do lote: "))
	    (while (setq ponto ( getpoint "\nClique em um ponto: ")) ;; primeiro while	
      		(command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\PONTO_LOTE.dwg" ponto escala "" "0" QUADRA CONJUNTO LOTE (strcat "P-" (itoa i)) "")
                (setq i (1+ i))
	    ) ;;fim do while
        );; FIM DO WHILE
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'ponto_lote' para iniciar")