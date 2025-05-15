(defun tira_ponto (TEXTO / AUX j txt )
  	(setq comp (strlen TEXTO))
        (setq j 0)
        (setq aux "")
        (repeat (strlen TEXTO)
	  	(setq txt (substr TEXTO (setq j(1+ j)) 1))
	        (cond
		  ((= txt ".") (setq AUX (strcat aux "," )))
	          ((/= txt ".") (setq AUX (strcat aux txt )))
		 )
	)
  	(setq aux aux)
 )
(defun c:pext ()
  (setq pref (getvar "dwgprefix"))
  (setq nomearq (getvar "dwgname"))
  (setq nomef (strcat pref nomearq ".txt"))
  (setq prefixo (getstring "Fonerça o Prefixo da numeração dos pontos: "))
  (setq tipo (getstring "Deseja colocar pontos no desenho: <SIM> ou <NAO> "))
  (setq nic (atoi(getstring "Fonerça o número inicial dos pontos: ")))
  (setq nic(- nic 1))
  (if (or (= tipo "SIM")(= tipo "sim"))
  (progn
	  (setq escala(getreal "Escala do desenho: "))
	  (setq escala (/ escala 1000.0))
	  ;;;;(if (=(findfile nomef) nomef) ((setq nomef (strcat pref "PolyExt" ".txt")))
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
									(setq lin (strcat (itoa i) ";" prefixo "-" (itoa (+ i nic)) ";" (rtos(car Coord) 2 11) ";" (rtos (cadr Coord) 2 11) ";" (rtos (last Coord) 2 11)     ))
									(setq lin (tira_ponto lin))
									;;(setq lin (strcat (itoa i) ";" "P-" (itoa i) ";" (rtos(car Coord) 2 11) ";" (rtos (cadr Coord) 2 11) ))
									(write-line lin f)
									(setq elemento (entnext elemento))
									(setq listassoc (entget elemento))
									(setq Objeto (cdr(assoc 0 listassoc)))
									(setq pt (list (car Coord) (cadr Coord)))
									(setq DESC (strcat prefixo "-" (itoa (+ i nic))))
									(command "_.layer" "m" "VERTICE" "")
									Z:\APOIO_TECNICO\Biblioteca CAD\Blocos\TOPOGRAFIA
									(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "Z:\\APOIO_TECNICO\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\SIMB_PONTO_1.dwg" pt escala "" "0" DESC)
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
									(setq lin (strcat (itoa i) ";" prefixo "-" (itoa (+ i nic)) ";" (rtos (car Coord) 2 11) ";" (rtos (cadr Coord) 2 11)";" (rtos (last Coord) 2 11)))
									;;;(setq lin (strcat (itoa i) ";" "P-" (itoa i) ";" (rtos (car Coord) 2 11) ";" (rtos (cadr Coord) 2 11)))
									(setq lin (tira_ponto lin))
									(write-line lin f)	       
									(setq listassoc (member assoc_10 listassoc))
									(setq listassoc (cdr listassoc))  
									(setq pt (list (car Coord) (cadr Coord)))
									(setq DESC (strcat prefixo "-" (itoa (+ i nic))))
									(command "_.layer" "m" "VERTICE" "")
									(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "Z:\\APOIO_TECNICO\\Biblioteca CAD\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\SIMB_PONTO_1.dwg" pt escala "" "0" DESC)						
								);;fim do progn
							);; fim do while
				   );;fim do progn
			  );;fim do if
		  );fim do progn
	  );;fim do if
	  ));; fim do if caso tipo = SIM
	  
	   
  (if (or (= tipo "NAO")(= tipo "nao"))
  (progn
	  ;;;;(if (=(findfile nomef) nomef) ((setq nomef (strcat pref "PolyExt" ".txt")))
	  (setq f (open nomef "a" ))
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
									(setq lin (strcat (itoa i) ";" prefixo "-" (itoa (+ i nic)) ";" (rtos(car Coord) 2 11) ";" (rtos (cadr Coord) 2 11) ";" (rtos (last Coord) 2 11)     ))
									;;(setq lin (strcat (itoa i) ";" "P-" (itoa i) ";" (rtos(car Coord) 2 11) ";" (rtos (cadr Coord) 2 11) ))
									(setq lin (tira_ponto lin))
									(write-line lin f)
									(setq elemento (entnext elemento))
									(setq listassoc (entget elemento))
									(setq Objeto (cdr(assoc 0 listassoc)))
									(setq pt (list (car Coord) (cadr Coord)))
									(setq DESC (strcat prefixo "-" (itoa (+ i nic))))
									(command "_.layer" "m" "VERTICE" "")
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\SIMB_PONTO_1.dwg" pt escala "" "0" DESC)
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
									(setq lin (strcat (itoa i) ";" prefixo "-" (itoa (+ i nic)) ";" (rtos (car Coord) 2 11) ";" (rtos (cadr Coord) 2 11)";" (rtos (last Coord) 2 11)))
									;;;(setq lin (strcat (itoa i) ";" "P-" (itoa i) ";" (rtos (car Coord) 2 11) ";" (rtos (cadr Coord) 2 11)))
									(setq lin (tira_ponto lin))
									(write-line lin f)	       
									(setq listassoc (member assoc_10 listassoc))
									(setq listassoc (cdr listassoc))  
									(setq pt (list (car Coord) (cadr Coord)))
									(setq DESC (strcat prefixo "-" (itoa (+ i nic))))
									(command "_.layer" "m" "VERTICE" "")
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\INCRA_vertice_V.dwg" pt escala "" "0" DESC)
									;;(command "_.insert" "C:\\GTOP\\APOIO\\Blocos\\TOPOGRAFIA\\SIMB_PONTO_1.dwg" pt escala "" "0" DESC)						
								);;fim do progn
							);; fim do while
				   );;fim do progn
			  );;fim do if
		  );fim do progn
	  );;fim do if
	  ));; fim do if caso tipo = NAO
(close f)
(princ (strcat "\n----->Informações escritas no arquivo " nomef "\n"))
(princ)
)
(princ "\nDigite 'pext' para iniciar...\n")