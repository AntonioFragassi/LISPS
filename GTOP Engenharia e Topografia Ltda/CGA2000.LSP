;;;Bug de parada já resolvido
;;;Lisp para separação dos atributos em layers distintos dos pontos gerados
;;;no TOPOGRAPH 98
;;; Modificado em 31/01/2001 por Fragassi
(defun c:cga98 ()
        (command "_.view" "S" "Curr")
	(setq SS1 (ssget)   N   (if SS1 (sslength SS1) 0))

  (setq ssl   (sslength ss1)  nsset (ssadd))
  (if (> ssl 25)
    (princ "\nVerificando as entidades, espere por favor. ")
  )
  (while (> ssl 0)
    (setq temp (ssname ss1 (setq ssl (1- ssl))))
    (if (= (cdr (assoc 0 (entget temp))) "INSERT") 
	(progn
		(setq name (substr (cdr (assoc 2 (entget temp))) 1 4))
		(if (= name "SIMB")  (ssadd temp nsset))
	) 
    )
  )
  (setq ssl (sslength nsset)  ss1 nsset   )
  (print ssl)
  (princ "blocos encontrados. ")
	(command "_.zoom" "E")
	(setq x 0)
        (while (< x N)
		(setq roda T) 
		(setq A (ssname SS1 x))
                                (setq  B (entget A))
                               (setq C (entnext (cdr (assoc -1 B))))
		(setq blklay (cdr (assoc 8 b)))
		(while roda
			(setq valor nil)
			(setq D (entget C))
		        (setq attlay (cdr (assoc 8 D)))
			(setq newlay (strcase (cdr (assoc 2 D))))
			(setq valor (cdr (assoc 1 D)))
			;;(if (= attlay "0") 
			    (progn
				(if (and (/= valor " ") (/= valor ""))  
					(progn
					        (command "_.layer" "m" newlay "")
					        (command "_.attedit" "Y" "SIMB*" newlay valor C "Layer" newlay "Color" "Bylayer" "")
					);;fim progn
				);;fim do if
			    );;fim progn
			    ;;(print "Bloco já editado...")
			;;);;fim do if
			(setq B (entget C))
			(setq C (entnext (cdr (assoc -1 B)))) 
			(if (= (cdr (assoc 0 (entget C))) "SEQEND") (setq roda nil)) 
		);;fim do enquanto
		(setq x (+ x 1))
	);;fim o while
 	(command "_.zoom" "E" )
)
(princ "\n****** USE SOMENTE COM ARQUIVOS DO TOPOGRAPH98 ****** \nDigite 'cga98' para iniciar")