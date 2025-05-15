;; Coloca simbolo de pontos com numeração sequencial

(defun c:VERTICE ( / olcmdecho escala ponto i PONTO DESC COTA)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
	(setq escala (getreal "Escala do desenho: "))
        (setq escala (/ escala 1000.0))
        (setq i (getint "Número inicial: "))
        (setq prefixo (getstring "Fonerça o Prefixo: "))
        ;(setvar "osmode" 1)
	(while (setq Pto_bloco (Getpoint "Selecione o Ponto: " )) ;; primeiro while  
	        (setq DESC (strcat prefixo (itoa i)))
	        (command "_.layer" "m" "VERTICE" "")
                ;(setvar "osmode" 0)
      		(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" Pto_bloco escala "" "0" DESC)
                (setq i (1+ i))
                ;(setvar "osmode" 1)
	) ;;fim do primeiro if
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'VERTICE' para iniciar")