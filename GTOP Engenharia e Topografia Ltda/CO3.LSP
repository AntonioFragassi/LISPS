(defun c:CO3()
(prompt "\nEscreve as coordenadas de um ponto (2D).")
    (initang)
    (ver_regua)
    (setq h (getvar "TEXTSIZE"))
    (setq pt (getpoint "\nAponte um ponto."))
    (setq E (car pt))
    (setq E1 (rtos E 2 3)) 
    (setq E1 strcat ( "E="  E1))
    (setq pt1 (getpoint "\nCoordenada N. Defina a origem."))
    (command "_.TEXT" pt1 h 0 E1)
    (setq N (cadr pt))
    (setq N1 (rtos N 2 3))
    (setq N1 strcat ( "N="  N1))
    (setq pt1 (cal "pt1-[0,1.5*h]"))
    (command "_.TEXT" pt1 h 0 N1)
    (princ)
)