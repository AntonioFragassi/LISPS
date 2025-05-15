;; Coloca simbolo de pontos com numeração sequencial
(defun c:bota_ponto ( / olcmdecho escala ponto i)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
	;(setq escala (getreal "Escala do desenho: "))
        ;(setq escala (/ escala 1000.0))
        (setq escala 0.0254)
        (setq i (getint "Número inicial: "))
	(while T ;; primeiro while
      		(setq ponto ( getpoint "\nClique em um ponto: "))
      		(command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" ponto escala "" "0" (strcat "P-" (itoa i)) "")
                ;(setq DESC (itoa i))
                ;(command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\SIMB_PONTO1.dwg" ponto escala "" "0" DESC)
                (setq i (1+ i))
	) ;;fim do primeiro if
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'Bota_ponto' para iniciar")