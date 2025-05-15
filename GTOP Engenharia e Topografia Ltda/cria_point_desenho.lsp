(defun c:pontos ()
  (setq olcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq oldosmode (getvar "osmode"))
   (setvar "osmode" 0)
  (if (setq Elemento (car(Entsel "\nClique polilinha a criar pontos:")))
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
				(command "_.point" Coord) 
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
                                (command "_.point" Coord)	       
			        (setq listassoc (member assoc_10 listassoc))
				(setq listassoc (cdr listassoc))    
	   		);;fim do progn
             	);; fim do while
               );;fim do progn
          );;fim do if

      );fim do progn
  );;fim do if
(princ)
 (setvar "cmdecho" olcmdecho)
 (setvar "osmode" oldosmode)
)
(princ "\nDigite 'pontos' para iniciar...\n")