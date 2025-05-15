;; Coloca simbolo de pontos com numeração sequencial

(defum LINHA ( / VETOR Elemento listassoc Assoc_10)
    (setq VETOR nil)
    (if (setq Elemento (car(Entsel "\nClique no eixo a exportar:")))
        (progn
	               (setq listassoc (entget elemento))
	               (setq Assoc_10 (assoc 10 listassoc))
	               (setq listassoc (member Assoc_10 listassoc))
	               (setq VETOR (append VETOR (list(cdr Assoc_10))))
	               (setq listassoc (cdr listassoc))
		       (while (assoc 10 listassoc)
		           (setq listassoc (member (assoc 10 listassoc) listassoc))
		           (setq Assoc_10 (assoc 10 listassoc))
	          	   (setq VETOR (append VETOR (list(cdr Assoc_10))))
	  		   (setq listassoc (cdr listassoc))
	               );;fim do while
    )

)

(defum NMAX ( VETOR / i maximo ponto)
    ;;;;(setq VETOR '((10 50) (30 90) (40 23)))
    (setq comp (length VETOR))
    (setq i 0)
    (wilhe (< i (- comp 1))
        (progn
              (setq maximo (last (nth i VETOR )))
              (if (< maximo (last (nth (+ i 1) VETOR )))
                   (progn
                      (setq maximo (last (nth (+ i 1) VETOR )))
                      (setq ponto i)
                    );; fim do progn
               );;fim if
        );;fim do progn
    );;fim do while

)


(defum ORDENA ( VETOR  ponto / VETOR_ORDENADO i Fator1 fator2)
    (setq VETOR_ORDENADO nil)
    (setq comp (length VETOR))
    (setq fator1 ponto)
    (setq fator2 (- comp ponto))
    (setq i 0)
    (wilhe (< i (- comp 1))
        (progn
              (setq maximo (last (nth i VETOR )))
              (if (< maximo (last (nth (+ i 1) VETOR )))
                   (progn
                      (setq maximo (last (nth (+ i 1) VETOR )))
                      (setq ponto i)
                    );; fim do progn
               );;fim if
        );;fim do progn
    );;fim do while

)


(defun c:VERTICE ( / olcmdecho escala ponto i PONTO DESC COTA)
	(setq olcmdecho (getvar "cmdecho"))
	(setvar "cmdecho" 0) 
	(setq escala (getreal "Escala do desenho: "))
        (setq escala (/ escala 1000.0))
        (setq i (getint "Número inicial: "))
        (setq prefixo (getstring "Fonerça o Prefixo: "))
        (setvar "osmode" 1)
	(while (setq Pto_bloco (Getpoint "Selecione o Ponto: " )) ;; primeiro while  
	        (setq DESC (strcat prefixo (itoa i)))
	        (command "_.layer" "m" "VERTICE" "")
                (setvar "osmode" 0)
      		(command "_.insert" "TC:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\SIMB_VERTICE.dwg" Pto_bloco escala "" "0" DESC)
                (setq i (1+ i))
                (setvar "osmode" 1)
	) ;;fim do primeiro if
	(setvar "cmdecho" olcmdecho)
	(princ)
)
(princ "\nDigite 'VERTICE' para iniciar")