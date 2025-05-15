(defun BLOCO ( i coord escala / DESC )
      (setq DESC (strcat prefixo (NPONTO i)))
      (command "_.layer" "m" "VERTICE" "")
      (command "_.insert" "T:\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\SIMB_VERTICE.dwg" coord escala "" "0" DESC)
)

(defun NPONTO ( VALOR / RESULTADO)
   (cond
     ( (< VALOR 10) (setq RESULTADO (strcat "000" (itoa VALOR))))
     ( (< VALOR 100) (setq RESULTADO (strcat "00" (itoa VALOR))))
     ( (< VALOR 1000) (setq RESULTADO (strcat "0" (itoa VALOR))))
     ( (< VALOR 10000) (setq RESULTADO (strcat "" (itoa VALOR))))
   )
  (setq RESULTADO RESULTADO)
)

(defun c:pext ()
   (setvar "osmode" 0)
  (setq pref (getvar "dwgprefix"))
  (setq nomef (strcat pref "PolyExt" ".txt"))
  (setq f (open nomef "w" ))
  (setq i 0)
  (setq olcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0) 
  (setq escala (getreal "Escala do desenho: "))
  (setq escala (/ escala 1000.0))
  (setq prefixo "V-")
  (if (setq Elemento (car(Entsel "\nClique polilinha a exportar:")))
     (progn 
          (setq listassoc (entget elemento))
          (setq Objeto (cdr(assoc 0 listassoc)))
          (if (= Objeto "POLYLINE")
              (progn
                        (setq elemento (entnext elemento))
                        (setq listassoc (entget elemento))
                        (setq Objeto (cdr(assoc 0 listassoc)))
                        (while (= Objeto "VERTEX")
                            (progn
			        (setq i (1+ i))
				(setq assoc_10 (cdr (assoc 10 listassoc)))
                                (setq Coord (cdr (assoc 10 listassoc)))
                                (BLOCO i Coord escala)
                                (setq lin (strcat (itoa i) ";" "V-" (NPONTO i) ";" (rtos(car Coord) 2 11) ";" (rtos (cadr Coord) 2 11)     ))
				(write-line lin f)
              			(setq elemento (entnext elemento))
                                (setq listassoc (entget elemento))
                                (setq Objeto (cdr(assoc 0 listassoc)))
	   		   );;fim do progn
             	);; fim do while
               );;fim do progn
          );;fim do if



          (if (= Objeto "LWPOLYLINE")
              (progn
                        (setq listassoc (entget elemento))
                        (while (setq assoc_10 (assoc 10 listassoc))
                           (progn
			        (setq i (1+ i))
			        (setq Coord (cdr assoc_10))
                                (BLOCO i Coord escala)
			        (setq lin (strcat (itoa i) ";" "V-" (NPONTO i) ";" (rtos (car Coord) 2 11) ";" (rtos (cadr Coord) 2 11) ))
				(write-line lin f)	       
			        (setq listassoc (member assoc_10 listassoc))
				(setq listassoc (cdr listassoc))    
	   		);;fim do progn
             	);; fim do while
               );;fim do progn
          );;fim do if

      );fim do progn
  );;fim do if
(close f)
(princ (strcat "\n----->Informações escritas no arquivo " nomef "\n"))
(princ)
)
(princ "\nDigite 'pext' para iniciar...\n")