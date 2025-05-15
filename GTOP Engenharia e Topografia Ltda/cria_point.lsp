(defun c:pontos ()
  (setq pref (getvar "dwgprefix"))
  (setq nomef (strcat pref "PolyExt" ".txt"))
  (setq f (open nomef "w" ))
  (setq i 0)
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
                                (setq lin (strcat "Point " (rtos(car Coord) 2 3) "," (rtos (cadr Coord) 2 3)      ))
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
			        (setq lin (strcat "Point " (rtos (car Coord) 2 3) "," (rtos (cadr Coord) 2 3)))
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
(princ "\nDigite 'pontos' para iniciar...\n")