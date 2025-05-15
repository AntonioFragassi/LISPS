;; coloca elevação em curvas
(defun c:ev ( / pt1 pt2 conjunto comp Val1 Equid i Elev ent1)
   (setq olcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (setq pt1 (getpoint "Primeiro ponto"))
   (print "")
   (setq pt2 (getpoint "Segundo ponto, no sentido de crescimento das curvas"))
   (print "")
   (setq conjunto (ssget "F" (list pt1 pt2)))
   (if conjunto
       (progn
		(setq comp (sslength conjunto))
		(setq comp (- comp 1))
                (initget 7)
                (setq Val1 (getreal "Forneça elevação inicial: "))
                (print "")
                (initget 7)
                (setq Equid (getreal "Forneça Equidistância Vertical: "))
                (print "")
		(setq i 0)
                (while (<= i comp)
			(setq Elev (+ Val1 (* i Equid)))
                        (setq ent1 (ssname conjunto i))
                        (command "_.change" ent1 "" "P" "E" Elev "")
			(command "_.change" ent1 "" "P" "c" 31 "")
                        (setq i (+ i 1))
		);; fim do enquanto
       );;fim do então
       (progn
		(print "Não foram encontradas polilinha")	
       );;fim do senão
   ); fim do if
   (setvar "cmdecho" olcmdecho)
   (princ "")
)
(print "Digite EV para iniciar")