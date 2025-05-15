;; Coloca simbolo de pontos com numeração sequencial

(defun NPONTO ( VALOR / RESULTADO)
   (cond
     ( (< VALOR 10) (setq RESULTADO (strcat "000" (itoa VALOR))))
     ( (< VALOR 100) (setq RESULTADO (strcat "00" (itoa VALOR))))
     ( (< VALOR 1000) (setq RESULTADO (strcat "0" (itoa VALOR))))
     ( (< VALOR 10000) (setq RESULTADO (strcat "" (itoa VALOR))))
   )
  (setq RESULTADO RESULTADO)
)

(defun c:INCRA ( / olcmdecho escala ponto i PONTO DESC COTA Tipo)
       (setq olcmdecho (getvar "cmdecho"))
        (setvar "cmdecho" 0) 
        (setq escala (getreal "Escala do desenho: "))
        (setq escala (/ escala 1000.0))
        (setq prefixo (getstring "Fonerça o Prefixo do Profissional: "))
	(setq Tipo (getstring "Deseja Inserir Marco <M>, Marco Virtual <V> ou Ponto <P>: "))
	(setq Tipo (strcase Tipo))
        ( cond
	   ( (= Tipo "P")
	     (progn
	         (command "_.layer" "m" "TIPO_P" "")
                 (setq i (getint "Número inicial: "))
	         (while T ;; primeiro while
      		     (setq ponto ( getpoint "\nClique em um ponto: "))
      		     (command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_P.dwg" ponto escala "" "0" (strcat prefixo "-P-" (NPONTO i)) "")
                     (setq i (1+ i))
                 );;fim do while
	     );fim do progn
	   );fim do primeiro cond
	   ( (= Tipo "M")
	     (progn
	         (command "_.layer" "m" "MARCO_CONCRETO" "")
                 (setq i (getint "Número inicial: "))
	         (while T ;; primeiro while
      		     (setq ponto ( getpoint "\nClique em um ponto: "))
      		     (command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_M.dwg" ponto escala "" "0" (strcat prefixo "-M-" (NPONTO i)) "")
                     (setq i (1+ i))
                 );;fim do while
	     );fim do progn
	   );fim do primeiro cond
	   ( (= Tipo "V")
	     (progn
	         (command "_.layer" "m" "MARCO_VIRTUAL" "")
                 (setq i (getint "Número inicial: "))
	         (while T ;; primeiro while
      		     (setq ponto ( getpoint "\nClique em um ponto: "))
      		     (command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" ponto escala "" "0" (strcat prefixo "-V-" (NPONTO i)) "")
                     (setq i (1+ i))
                 );;fim do while
	     );fim do progn
	   );fim do primeiro cond
         )       
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'INCRA' para iniciar")