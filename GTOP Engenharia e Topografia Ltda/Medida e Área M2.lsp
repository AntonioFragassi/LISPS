;;;	AUTOB4.LSP por Abner Lima de Oliveira - TOPOCART
;;;	Versão 1.0 - 24/11/98
;;;	Rotina para cotagem e determinação de áreas de loteamentos. Calcula áreas e cota 
;;;	divisas após o usuário apontar um ponto dentro do lote.
;;;	Esta rotina é distribuída gratuitamente. Por isso não há garantias contra erros
;;;	ou contra os possíveis danos causados por estes. O Suporte a este produto será 
;;;	fornecido pelo e-mail "topocart@apis.com.br" de acordo com a disponibilidade do
;;;	autor.
;;;	O Usuário desta rotina tem direito de tirar proveito dela inclusive em programas
;;;	próprios e se compromete a não omitir a fonte nem o autor.
;;;	Quaisquer "bugs" ou sugestões devem ser comunicados ao autor no endereço acima.
;;;
;;;	Foi escrita para AutoCAD r14, pressupõe a carga prévia da calculadora geométrica
;;;	(CAL no AutoCAD, e GEOMCAL com arxload) e trabalha em conjunto com o menu TOPOCART
;;;	de Fabrício Leal, do qual aproveita as funções Kr e Régua/Escala. Créditos para Rogério
;;;	AA, autor da rotina de cálculo de Áreas.
;;;
;;;____________________________
;;;Comando: 'mb'
;;;
;;;
;;;Para implementar:    - Impedimento de pular quadras.


(defun capaerre ()                                  ;;; Do Lisp "Topo" de Fabrício M. B. Leal
    (if (< (GETVAR "USERR1") 1.0)
      (progn
	(setq KR 1.0000000)
	(princ "\nKR nao cadastrado!!!")
	(princ (strcat "\nEntre com KR <" (rtos KR 2 7) "> : "))
	(setq temp KR)
	(setq KR (getreal))
	(if (= KR nil) (setq KR temp))
	(princ (strcat "\nKR atual ==> " (rtos KR 2 7)))
	(SETVAR "USERR1" KR)
      )
    )

    (if (not KR)
      (progn
	(setq KR (GETVAR "USERR1"))
	(setq hakr 1)
      )
    )

    (princ (strcat "\nKR atual ==> " (rtos KR 2 7)))
    (princ)
)


(defun mb ()
      (setq Schlist (list))
      (prompt "\nIndique ponto interno ao lote:")
      (setq ptcir (getpoint))
      (if ptcir
	      (progn
	      (setq ptcir (list (car ptcir) (cadr ptcir)))
	      (setvar "clayer" "0")
	      (command "_.-bhatch" "a" "b" "e" "s" "i" "i" "n" "" "" ptcir "")
	      (setq hatx (entlast))
	      (setq enthatx (entget hatx))
		(if (/= (cdr (assoc 0 enthatx)) "HATCH") (alert "Não é possível criar o hatch")
		(progn
	      (command "_.erase" hatx "")

	      (setq numvert (cdr (assoc 93 enthatx)))
	      (setq count 17)
	      (setq tip (car (nth count enthatx)))
	      (while (/= tip 97)
		(if (= tip 10)
		(progn
		(setq part (nth count enthatx))
		(setq vertix (cdr part))
		(setq Schlist (append vertix Schlist))
		)
		)
	      (setq count (1+ count))
	      (setq tip (car (nth count enthatx)))
      	      )

     	      (setq Nthh 0)
	      (setq Ntimes (/ (length Schlist) 3))
		(setq Totx 0
		        Toty 0)
	      (repeat Ntimes
			(setq xnew (nth Nthh Schlist)) 
			(setq Totx (+ Totx xnew)) 
			(setq ynew (nth (1+ Nthh) Schlist))
			(setq Toty (+ Toty ynew))
			(setq Nthh (+ Nthh 3))
      	      )
	      (setq Medx (/ Totx Ntimes))		
	      (setq Medy (/ Toty Ntimes))
	      (setq MedPt (list Medx Medy))


	      (setq Nthh 0)
	      (setq Ntimes (/ (length Schlist) 3))
	      (setq vertold nil)
	      (setq Further 0.0)

	      (repeat Ntimes
			(setq xnew (nth Nthh Schlist)) 
			(setq ynew (nth (1+ Nthh) Schlist))
			(setq vertnew (list xnew ynew))

			(if vertold (progn 
				(Writ vertold vertnew)
				(setq Far (abs (distance vertold vertnew)))0
	      			(princ "\n")
				(princ Far)
				(setq FarAng (angle vertold vertnew))
				(princ "\n")
				(princ FarAng)
			             )
			(setq Privert vertnew)
			)
			(if vertold (progn
					(if (> Far Further)(progn
				           (setq Further Far)
				           (setq FurAng (/ (* 180 (angle vertold vertnew)) 3.1415926535))
			        	   (if (and (> FurAng 90) (< FurAng 270)) (setq FurAng(+ FurAng 180)))
				           )
					)
			     		)
			)
			(setq vertold vertnew)
			(setq Nthh (+ Nthh 3))
	      )
	      (Writ vertnew Privert)

	;;;Cálculo da área (Rogério AA)
	        (setq Nthh 0
		      Barea 0.0)
      		(setq Ntimes (1- (/ (length Schlist) 3)))

      		(repeat Ntimes
			(setq E1 (nth Nthh Schlist)) 
			(setq N1 (nth (1+ Nthh) Schlist))
			(setq E2 (nth (+ 3 Nthh) Schlist)) 
			(setq N2 (nth (+ 4 Nthh) Schlist))
	        	(setq A (* 0.5 (* (+ N2 N1) (- E1 E2))))
	        	(setq Barea (+ Barea A))
			(setq Nthh (+ 3 Nthh))

      		)
		(setq E1 (nth (* Ntimes 3) Schlist)) 
		(setq N1 (nth (+ (* Ntimes 3) 1) Schlist))
		(setq E2 (nth 0 Schlist)) 
		(setq N2 (nth 1 Schlist))
       		(setq A (* 0.5 (* (+ N2 N1) (- E1 E2))))

      
       		(setq Barea (+ Barea A))


		(setq Barea (/ Barea (* kr kr)))

		(setq Barea (strcat (rtos (abs Barea) 2 2) "m2"))
		(setvar "clayer" "TXT-AREA")
		(command "_.text" "J" "MC" MedPt TxTH FurAng Barea)
	)
	)
	))
   )




(defun Quad (Anglu)

	(cond
	((and (> Anglu 0) (< Anglu 91)) (setq Qad 1))
	((and (> Anglu 91) (< Anglu 181)) (setq Qad 2))
	((and (> Anglu 181) (< Anglu 271)) (setq Qad 3))
	((and (> Anglu 271) (< Anglu 360)) (setq Qad 4))
	((= Anglu 0) (setq Qad 1))
	)
)


(defun Writ (vert1 vert2)
    (setq Divisa (rtos (/ (distance vert1 vert2) kr) 2 2))
    (setq Ptext (cal "(vert2+vert1)/2"))
    (setq Tangle (rtd (angle vert1 vert2)))
    (setq Pangle (rtd (angle MedPt Ptext)))
	   (setq paux (cal "(MedPt+Ptext)/2"))

	(Quad Pangle)
	(setq QuadP Qad)
	(Quad Tangle)
	(setq QuadT Qad)

	(cond 
	((= QuadP 1) (progn
		     (if (= QuadT 1) (progn
				     (setq Justie "TC")
				     )
		     )
		     (if (= QuadT 3) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     (if (= QuadT 4) (progn
				     (setq Justie "TC")
				     )
		     )
		     )
	) 
	((= QuadP 2) (progn
		     (if (= QuadT 1) (progn
				     (setq Justie "TC")
				     )
		     )
		     (if (= QuadT 2) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     )
	) 
	((= QuadP 3) (progn
		     (if (= QuadT 1) (progn
				     (setq Justie "TC")
				     )
		     )
		     (if (= QuadT 2) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     (if (= QuadT 3) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     )
	)
	((= QuadP 4) (progn
		     (if (= QuadT 2) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     (if (= QuadT 3) (progn
				     (setq Justie "BC")
				     (setq Tangle (+ Tangle 180))
				     )
		     )
		     (if (= QuadT 4) (progn
				     (setq Justie "TC")
				     )
		     )
		     )
	) 
	)	

	(setq P1 (cal "Ptext+[TxTH,TxTH]"))
	(setq P2 (cal "Ptext-[TxTH,TxTH]"))
	(setq Whoisthere (ssget "C" P1 P2))
	(if Whoisthere
		(progn
		(setq Howmany (sslength Whoisthere))
		(setq CanWrite T)
		(while (> Howmany 0)
			(setq Howmany (1- Howmany))
			(setq Who (ssname Whoisthere Howmany))
			(setq EntWho (entget Who))
			(setq TypWho (cdr (assoc 0 (entget Who))))
			(setq PtWho (cdr (assoc 11 (entget Who))))
			(setq TexWho (cdr (assoc 1 (entget Who))))
			(if (= TypWho "TEXT")
				(if (< (distance PtWho Ptext) (/ TxTH 2.7))
						(if (= TexWho Divisa) (setq CanWrite nil))
					)
			)
		)
		(if (= Justie "TC") (setq ptext (polar ptext (/ (* 3.1415926535 (- Tangle 90)) 180) (/ TxTH 3))))
		(if CanWrite (progn
			     (setvar "clayer" "TXT-DIMENSOES")
			     (command "_.text" "J" Justie Ptext TxTH Tangle Divisa)
			     )
		)
		)
	)
)


(defun cht_er (s)                   ; If an error (such as ESC) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (if (= s "bad argument type")
	(PROGN
	(PRINC "\NERRO, REINICIANDO...")
	(reinit)
	)
      )
    )
    (eval(read U:E))
    (if cht_oe                        ; If an old error routine exists
      (setq *error* cht_oe)           ; then, reset it 
    )
    (princ)
  )

(defun reinit () (mb))

(defun c:mb () 
   (setq oldimzin (getvar "dimzin"))
   (setvar "dimzin" 1)
   (setq olcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 1)
   (setq oldosmode (getvar "osmode"))
   (setvar "osmode" 0)
   (setq oldangbase (getvar "angbase"))
   (setvar "angbase" 0)
   (setq oldangdir (getvar "angdir"))
   (setvar "angdir" 0)
   (setq oldpdmode (getvar "pdmode"))
   (setvar "pdmode" 0)
   (capaerre)
   (if (= ESCALA nil) (xx))

   (setq TxTH (getvar "textsize"))
   (command "_.-layer" "m" "TXT-DIMENSOES" "m" "TXT-AREA" "")
   (setvar "clayer" "0")

   (command "regenauto" "On")
   (command "_.-layer" "On" "0" "t" "0" "s" "0" "")
		(command "_.-layer" "t" Numlayer "") 
		(command "_.-layer" "On" Numlayer "")
   (setq ptcir T)
   (while ptcir (mb))   

   (command "_.-layer" "t" Numlayer "On" Numlayer "")
   (command "_.-layer" "On" "TXT-DIMENSOES" "On" "TXT-AREA" "")
   (setvar "dimzin" oldimzin)
   (setvar "cmdecho" olcmdecho)
   (setvar "osmode" oldosmode)
   (setvar "angbase" oldangbase)
   (setvar "angdir" oldangdir)
   (setvar "pdmode" oldpdmode)
)


(princ "\nDigite 'mb' para iniciar...")