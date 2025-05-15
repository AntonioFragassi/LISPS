(defun c:Tira_nos ()
  (setq olcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq oldosmode (getvar "osmode"))
   (setvar "osmode" 0)
  ;(setq Escala (getint "Valor da Escala do projeto: " ) )
  (setq Escala 500.0 )
  (setq Dist_minima (/ (* 0.030 Escala ) 100))
  (Princ "\n Selecione as curvas a diminuir vértices: " )
  (if (setq  Conjunto (ssget '((0 . "LWPOLYLINE"))))
    (progn
        (command "_.-Layer" "m" "Curvas_auxiliares" "")
        (setq Comp_Conj (sslength Conjunto))
        (while (> Comp_Conj 0)
	        (setq Lista_pontos nil)
	        (setq Comp_Conj (1- Comp_Conj))
	        (setq elemento (ssname Conjunto Comp_Conj))
                (setq Objeto_1 (ssname Conjunto Comp_Conj))
                (command "_.change" Objeto_1 "" "P" "c" 2 "")
  		(setq listassoc (entget elemento))
  		(setq Elevacao (list (cdr (assoc 38 listassoc))))
  		(setq assoc_10 (cdr (assoc 10 listassoc)))
  		(setq Pt0 (append assoc_10 elevacao))
	        (setq fechada (cdr(assoc 70 listassoc)))
	        (if ( = fechada 1)
		    (Setq Ptini Pt0)
		)
  		(setq Lista_pontos (append Lista_pontos (list Pt0)))
  		(setq listassoc (cdr (member (assoc 10 listassoc) listassoc)))
  		(while assoc_10
    			(setq assoc_10 (cdr (assoc 10 listassoc)))
    			(setq listassoc (cdr (member (assoc 10 listassoc) listassoc)))
	  		(if (assoc 10 listassoc)
		    		(progn
    					(setq Pt1 (append (cdr (assoc 10 listassoc))Elevacao))
    					(Setq Dist (distance Pt0 Pt1))
    					(if (> Dist Dist_minima)
        		   			(progn
            						(setq Lista_pontos (append Lista_pontos (list Pt1)))
	    						(setq Pt0 Pt1)
	 		   			)
      		         		)
		    		)
		 	 )
  		)
  		(setq Lista_pontos (append Lista_pontos (list Pt1)))
	        (if ( = fechada 1)
		    (setq Lista_pontos (append Lista_pontos (list Ptini)))
		)
  		(setq Tamanho (length Lista_pontos))
	        (if (> tamanho 1)
		  (progn
  			(setq cont 0)
  			(setq Pt0 (nth 0  Lista_pontos))
  			(while  (< Cont Tamanho)
      				(setq cont (+ 1 cont))
	      			(setq Pt1 (nth cont  Lista_pontos))
			        (if (/= Pt0 Pt1)
				  (progn
	        			(command "_.line" Pt0 Pt1 "")
	        		        (setq Pt0 Pt1)
				   )
				)
    			)
			(setq  Linhas (ssget "X" '((8 . "Curvas_auxiliares") (0 . "LINE"))))
                	(command "_.pedit" "m" Linhas "" "" "J" "" "")
	        	(setq  Linhas (ssget "X" '((8 . "Curvas_auxiliares") (0 . "LWPOLYLINE"))))
                	(command "_.pedit" "m" Linhas "" "s" "" )
	        	(setq Pt1 (nth 1  Lista_pontos))
	        	(setq  elemento (ssget "X" '((8 . "Curvas_auxiliares") (0 . "POLYLINE"))))
	        	(if (> Tamanho 3)
	            		(command "_.break" Linhas Pt1 Pt1 "")
			) 
		  )
		)
 
      	)
      (setq  elemento (ssget "X" '((8 . "Curvas_auxiliares"))))
      (command "_.convert" "P" "S" elemento "")
      ;(command "_.change" curva_base "" "P" "c" 2 "")
    )
  )
 (setvar "cmdecho" olcmdecho)
 (setvar "osmode" oldosmode)
)
(princ "\n")
(prompt "Digite 'Tira_nos' para diminuir vértices a longo da polyline")












