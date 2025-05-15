

(defun Encotra_point ( Val_E Val_N i Delta / Res Pt1 Pt2 ObjAux  Comp_Conj continua Atr_Ponto Atr_Desc Atr_Cota Ponto XX Set_Unit Set_Unit_Obj Pt_aux1 dist)
        (setq Ponto (list Val_E Val_N 0))
        (setq Res nil)
        (setq Pt1 (c:cal "[-Delta,+Delta,0] + Ponto"))
        (setq Pt2 (c:cal "[+Delta,-Delta,0] + Ponto"))
        (command "_.zoom" "W" Pt1 Pt2 )
  (if (setq ObjAux (ssget "W" Pt2 Pt1 '((0 . "POINT"))))
    (progn
        (setq Comp_Conj (sslength ObjAux))
        (cond
	  ((or (= Comp_Conj 0) (= Comp_Conj nil))
	       (progn
		 	(Setq Erro (strcat "Não consegui encontrar o Ponto " (itoa i) " da polilinha" ))
		        (print Erro)
		        (setq Ponto (list Val_E Val_N 0 0 (itoa i) ))
		        (command "_.circle" (list Val_E Val_N 0) "5" "")
		)
	   );; fim do primeiro cond
	  ((= Comp_Conj 1)
	        (progn 
		  	(setq ObjAux (ssname ObjAux 0))
		        (setq Atr_Cota (last (cdr(assoc 10 (entget ObjAux)))))
		        (setq Ponto (list Val_E Val_N Atr_Cota "PONTO" (itoa i) ))
		        
 		);;fim do progn do cond 2
	   );; fim do cond 2
	  (> Comp_Conj 1)
	  	(progn
                        (setq Comp_Conj (sslength ObjAux))
                        (setq XX T)
                        (while (and (> Comp_Conj 0) (= XX T))
	       			(setq Comp_Conj (1- Comp_Conj))
	       			(setq Set_Unit (ssname ObjAux Comp_Conj))
	       			(setq Set_Unit_Obj (entget Set_Unit))
                                (setq Pt_aux1 (cdr (assoc 10 Set_Unit_Obj)))
                                (setq dist (distance Pt_aux1 Ponto))
				(if (= dist 0)
					(progn
                                        	(setq ObjAux Set_Unit)
					        (setq Atr_Cota (last (cdr(assoc 10 (entget ObjAux)))))
		        			(setq Ponto (list Val_E Val_N Atr_Cota "PONTO" (itoa i) ))
						(setq XX nil)
					);;fim do progn
                                );;fim do if
                        );;fim do while
                        (command "_.circle" (list Val_E Val_N 0) "20" "")
		        (if (< Comp_Conj 0)
			   (progn
			    (setq continua nil)
			    (Setq Erro (strcat "Não consegui encontrar o Ponto " (itoa i) " da polilinha" ))
			    (print Erro)
			    (setq Ponto (list Val_E Val_N 0 0 (itoa i) ))
			    (command "_.circle" (list Val_E Val_N 0) "5" "")
			   )
			)
		);; fim do progn do cond 3
	 );;fim do cond
      );;fim do progn
      (progn
      		((setq Ponto (list Val_E Val_N 0 0 (itoa i))))
	        (command "_.circle" (list Val_E Val_N 0) "5" "")
	)
    )
         (setq Res (list Ponto))
 );; fim da função busda POINT






(defun Coloca_Texto (Perfil Ponto1 / i Cota Prog Desc Ht P1 P2 P3 P4 P5 Num_vert)
        (command "_.-Layer" "m" "TEXTO_EIXO" "")
        (setq Ht (getvar "textsize"))
  	(setq Num_vert (length Perfil))
        (setq i 0)
   	(while (< i Num_vert)
        	(setq P1 (car (nth i Perfil)))
        	(setq prog (nth 0 P1))
        	(setq Cota (/(nth 1 P1) 10.0))
        	(setq Desc (Last (nth i Perfil)))
        	(setq P3 (c:cal "Ponto1 - 2*[0.0,Ht,0.0] + [Prog,0.0,0.0]"))
  		(setq P4 (c:cal "Ponto1 - 4*[0.0,Ht,0.0]+ [Prog,0.0,0.0]"))
        	(setq P5 (c:cal "Ponto1 - 6*[0.0,Ht,0.0]+ [Prog,0.0,0.0]"))
        	(command "_.text" P3 "" "0" Desc)
        	(command "_.text" P4 "" "0" (rtos prog 2 3))
        	(command "_.text" P5 "" "0" (rtos Cota 2 3))
      		(setq i (+ i 1))
 	);;fim do while
  
        ;;(entmake '((0 . "POLYLINE") (8 . "EIXO") (62 . 1) (38 . 0.0) (10 4.0 4.0 ) (10 5.0 20.0) (10 20.0 100.0)))
 );;fim da Função escreve polilinha



(defun Desenha_eixo (Perfil Ponto1 Delta / i Cota Prog Desc Ht P1 P2 P3 P4 P5 Num_vert)
        (command "_.-Layer" "m" "EIXO" "")
        (setq Ht (getvar "textsize"))
  	(setq Num_vert (length Perfil))
        (setq i 0)
        (setq P1 (car (nth i Perfil)))
        (setq P2 (nth 1 P1))
        (setq Delta (- P2 Delta (* 20 Ht)))
        (setq P1 (c:cal "Ponto1 + P1 - [0.0,Delta,0.0]"))
        (setq i 1)
   	(while (< i Num_vert )
      		(setq P2 (car(nth i Perfil)))
	        (setq P2 (c:cal "Ponto1 + P2 - [0.0,Delta,0.0]"))
	        (command ".line" P1 P2 "")
	        (setq P1 P2)
      		(setq i (+ i 1))
 	);;fim do while
 );;fim da Função escreve polilinha

(defun cabecalho (Pto ht Prog / Ptins P1)

        (command "_.-Layer" "m" "TXT_EIXO" "")
        (setq Ptins (c:cal "Pto + 2*[0,-ht,0]+12*[-ht,0,0]"))
	(command "_.text" Ptins "" "0" "PONTOS")
        (setq Ptins (c:cal "Pto + 4*[0,-ht,0]+12*[-ht,0,0]"))
	(command "_.text" Ptins "" "0" "PROGRESSIVA")
        (setq Ptins (c:cal "Pto + 6*[0,-ht,0]+12*[-ht,0,0]"))
	(command "_.text" Ptins "" "0" "COTAS")
        (setq P1 (c:cal "Pto + [Prog,0,0]"))
        (command ".line" Pto P1 "")
        ;(setq Ptins (c:cal "Pto + 4*[0,-ht,0]"))
	;(command "_.text" Ptins "" "0" "0.00")

  )

(defun dados_eixo (Pto ht Lista_dados Delta Exagero / Num_pontos Separar i Ponto1 Prog Cota)
  ;;Pto: é o ponto de inserção; Ht: altura dos textos 
  ;;Estrutura de dados da Lista_Dados [E,N,H,Ponto,Desc,Dist,Prog] vetor de 7 unidades
 	(setq Separar (* Delta 100))
 	(setq Pt_base (c:cal "[0,Separar,0] + Pto"))
 	(setq Num_pontos (length Lista_dados))
 	(setq i 0)
        (setq Prog 0)
 	(while (< i (- Num_pontos 1))
      		(setq Ponto1 (nth i Lista_dados))
	        (setq Prog (nth 5 Ponto1))
      		(setq Cota (* (nth 2 Ponto1) Exagero))
      		(setq i (+ i 1))
 	);;fim do while
        (command "_.-Layer" "m" "EIXO" "")
        (setq Ponto1 (nth (- (length Lista_dados) 1) Lista_dados))
     	(setq Ptins (c:cal "Ponto_re + [0,-ht,0]+ [0,-ht,0]"))
	(command "_.text" Ptins "" "0" (nth 3 Ponto1)) 


 )

(defun Leroy ( / Ht regua )
  (princ "\nRéguas: <40>;<50>;<60>;<80>;<100>;<120>;<140>;<175>;<200>;<240>;<290>;<350>;<425>;<450>")
  (initget 7)
  (setq regua (getint "\nForneça a régua do desenho: "))
  (command "_.STYLE" "ROMANS" "ROMANS" "0" "1.0" "0" "" "" "")
  (cond
     	((= regua 40) (setq Ht 1.0))
     	((= regua 50) (setq Ht 1.3))
     	((= regua 60) (setq Ht 1.6))
     	((= regua 80) (setq Ht 2.0))
   	((= regua 100) (setq Ht 2.5))
        ((= regua 120) (setq Ht 3.0))
        ((= regua 140) (setq Ht 3.5))
        ((= regua 175) (setq Ht 4.0))
        ((= regua 200) (setq Ht 4.5))
        ((= regua 240) (setq Ht 6.0))
        ((= regua 290) (setq Ht 7.5))
        ((= regua 350) (setq Ht 9.0))
        ((= regua 425) (setq Ht 11.0))
        ((= regua 450) (setq Ht 13.0))
        (T otherwise (setq Ht 1.0))
  )
  (setq Ht (* Ht (/ Escala 1000.0)))
  (setvar "TEXTSIZE" Ht)
);;fim da função leroy


(defun Encotra_Irrad ( Val_E Val_N i Delta / Res Pt1 Pt2 ObjAux  Comp_Conj continua Atr_Ponto Atr_Desc Atr_Cota Ponto XX Set_Unit Set_Unit_Obj Pt_aux1 dist)
        (setq Ponto (list Val_E Val_N 0))
        (setq Res nil)
        (setq Pt1 (c:cal "[-Delta,+Delta,0] + Ponto"))
        (setq Pt2 (c:cal "[+Delta,-Delta,0] + Ponto"))
        (command "_.zoom" "W" Pt1 Pt2 )
  (if (setq ObjAux (ssget "W" Pt2 Pt1 '((0 . "INSERT"))))
    (progn
        (setq Comp_Conj (sslength ObjAux))
        (cond
	  ((or (= Comp_Conj 0) (= Comp_Conj nil))
	       (progn
		 	(Setq Erro (strcat "Não consegui encontrar o IRRAD no Ponto " (itoa i) " da polilinha" ))
		        (print Erro)
		        (setq Ponto (list Val_E Val_N 0 0 (itoa i) ))
		        (command "_.circle" (list Val_E Val_N 0) "5" "")
		)
	   );; fim do primeiro cond
	  ((= Comp_Conj 1)
	        (progn 
		  	(setq ObjAux (ssname ObjAux 0))
		        (setq Atr_Cota (last (cdr(assoc 10 (entget ObjAux)))))
                	(setq ObjAux (entnext ObjAux))
                	(setq Atr_Ponto (cdr(assoc 1 (entget ObjAux))))
                	(setq ObjAux (entnext ObjAux))
                	(setq Atr_Desc (cdr(assoc 1 (entget ObjAux))))
		        (setq Ponto (list Val_E Val_N Atr_Cota Atr_Ponto Atr_Desc ))
		        
 		);;fim do progn do cond 2
	   );; fim do cond 2
	  (> Comp_Conj 1)
	  	(progn
                        (setq Comp_Conj (sslength ObjAux))
                        (setq XX T)
                        (while (and (> Comp_Conj 0) (= XX T))
	       			(setq Comp_Conj (1- Comp_Conj))
	       			(setq Set_Unit (ssname ObjAux Comp_Conj))
	       			(setq Set_Unit_Obj (entget Set_Unit))
                                (setq Pt_aux1 (cdr (assoc 10 Set_Unit_Obj)))
                                (setq dist (distance Pt_aux1 Ponto))
				(if (= dist 0)
					(progn
                                        	(setq ObjAux Set_Unit)
					        (setq Atr_Cota (last (cdr(assoc 10 (entget ObjAux)))))
					        (setq ObjAux (entnext ObjAux))
                				(setq Atr_Ponto (cdr(assoc 1 (entget ObjAux))))
                				(setq ObjAux (entnext ObjAux))
                				(setq Atr_Desc (cdr(assoc 1 (entget ObjAux))))
		        			(setq Ponto (list Val_E Val_N Atr_Cota Atr_Ponto Atr_Desc ))
						(setq XX nil)
					);;fim do progn
                                );;fim do if
                        );;fim do while
		        (if (< Comp_Conj 0)
			   (progn
			    (setq continua nil)
			    (Setq Erro (strcat "Não consegui encontrar o IRRAD no Ponto " (itoa i) " da polilinha" ))
			    (print Erro)
			    (setq Ponto (list Val_E Val_N 0 0 (itoa i) ))
			    (command "_.circle" (list Val_E Val_N 0) "5" "")
			   )
			)
		);; fim do progn do cond 3
	 );;fim do cond
      );;fim do progn
      (progn
      		((setq Ponto (list Val_E Val_N 0 0 (itoa i))))
	        (command "_.circle" (list Val_E Val_N 0) "5" "")
	)
    )
         (setq Res (list Ponto))
 );; fim da função busda Irrad

(defun Escala_bloco (Escala / blockset lentset temp entemp pt)
        (if (setq blockset (ssget "X" '((0 . "INSERT"))))
		(progn
			(setq lentset (sslength blockset))
			(while (> lentset 0)
			        ;(princ "/n/")
				(setq lentset (1- lentset))
				(setq temp (ssname blockset lentset))
				(setq entemp (entget temp))
				(setq pt (cdr (assoc 10 entemp)))
				(command "_.Scale" temp "" pt Escala)
			        ;(princ "/n-")
			)
		  )
	)
);; dim da função escala blocos

(defun c:PERF ( / )
 ;; Ponto_re olcmdecho oldosmode Log_File pref dwg nomedwg nomef Log_File Escala Exagero Delta i Lista_blocos Elemento listassoc
		         ;;Assoc_10 Val_E	Val_N Aux Num_pontos Prog Lista_dados  Ponto1 aux1 Ponto2 aux2  Dist Prog DN Linha Ht tipo
          (CAL "1+1")
	  (setq olcmdecho (getvar "cmdecho"))
	  (setvar "cmdecho" 0)
	  (setq oldosmode (getvar "osmode"))
	  (setvar "osmode" 0)
	  (setq oldpdmode (getvar "pdmode"))
	  (setvar "pdmode" 0)
	  (setq Log_File nil)
	  (setq pref (getvar "dwgprefix"))
	  (setq dwg (getvar "dwgname"))
	  (setq nomedwg (substr dwg 1 (- (strlen dwg) 4 )))
	  (setq nomedwg (strcat nomedwg ".txt"))
	  (setq nomef (strcat pref nomedwg))
	  (setq Log_File (open nomef "w"))
	  (setq Escala (getint "\nValor da Escala do projeto: " ) )
	  ;(setq Exagero (getint "\nForneça o exagero vertical: " ) )
          (setq tipo (getint "\nProcura Irrad <1> ou Point <2>?  Escolha o tipo: " ) )
          (setq Exagero 1.0 )
	  (setq Delta 0.003);;intervalo entre o ponto para busca de irrad
	  (Leroy)
          (Print "*******************************************************")
          (Print "*****    Espere Um pouco. Escalonando Bloco.  *********")
          (Print "*******************************************************")
	  (setq i 1)
	  (setq Lista_blocos nil)
          (command "_.zoom" "E" "" )
	  (if (setq Elemento (car(Entsel "\nClique no eixo a exportar:")))
	     (progn ;;monta uma lista chamada Lista_blocos contendo E,N,Cota, Atibuto Ponto; Atibuto Descrição para cara Irrad encontrado
	            ;;Lista_Blocos [E,N,H,Ponto,Desc] vetor de 5 unidades
                 (cond 
                   ((= tipo 1)
                    (progn
                       (Escala_bloco 0.001)
	               (setq listassoc (entget elemento))
	               (setq Assoc_10 (assoc 10 listassoc))
	               (setq listassoc (member Assoc_10 listassoc))
	               (setq Val_E (car (cdr Assoc_10 )))
	               (setq Val_N (last Assoc_10 ))
	               (Setq Aux (Encotra_Irrad Val_E Val_N i Delta))
	               (setq Lista_blocos (append Lista_blocos Aux))
	               (setq listassoc (cdr listassoc))
		       (while (assoc 10 listassoc)
		           (setq listassoc (member (assoc 10 listassoc) listassoc))
		           (setq Assoc_10 (assoc 10 listassoc))
		           (setq Val_E (car (cdr Assoc_10 )))
	          	   (setq Val_N (last Assoc_10 ))
	          	   (setq i (+ 1 i))
	          	   (Setq Aux (Encotra_Irrad Val_E Val_N i Delta))
	          	   (setq Lista_blocos (append Lista_blocos Aux))
	  		   (setq listassoc (cdr listassoc))
	               );;fim do while
                       (Escala_bloco 1000)
                     ));fim do progn tipo 1
                   ((= tipo 2)
                    (progn
	               (setq listassoc (entget elemento))
	               (setq Assoc_10 (assoc 10 listassoc))
	               (setq listassoc (member Assoc_10 listassoc))
	               (setq Val_E (car (cdr Assoc_10 )))
	               (setq Val_N (last Assoc_10 ))
	               (Setq Aux (Encotra_point Val_E Val_N i Delta))
	               (setq Lista_blocos (append Lista_blocos Aux))
	               (setq listassoc (cdr listassoc))
		       (while (assoc 10 listassoc)
		           (setq listassoc (member (assoc 10 listassoc) listassoc))
		           (setq Assoc_10 (assoc 10 listassoc))
		           (setq Val_E (car (cdr Assoc_10 )))
	          	   (setq Val_N (last Assoc_10 ))
	          	   (setq i (+ 1 i))
	          	   (Setq Aux (Encotra_point Val_E Val_N i Delta))
	          	   (setq Lista_blocos (append Lista_blocos Aux))
	  		   (setq listassoc (cdr listassoc))
	               );;fim do while
                     ));fim do progn tipo 2
                  );fim do cond
	      );fim do progn
	  );;fim do if

     	(setq Num_pontos (length Lista_blocos))
 	(setq i 0)
        (setq Prog (Getreal "\nForneça a última progressiva: "))
 	(setq Lista_dados nil)
        (setq Perfil nil)
        (setq Textos nil)
        (setq Maior 0.0)
        (setq Menor 100000.0)
 	(while (< i (- Num_pontos 1))
	  ;;Cria uma lista chamada Lista dados igual a Lista Blocos, contendo Diferença de Nível e progressivas fim
	  ;;Lista_Dados [E,N,H,Ponto,Desc,Dist,DN,Prog] vetor de 8 unidades
	  ;;Perfil: Lista contendo [Progressiva, Cota*10; 0.0] da sequência dos pontos
		(setq Ponto1 (nth i Lista_blocos))
        	(setq aux1 (list (nth 0 Ponto1) (nth 1 Ponto1) 0))
        	(setq Ponto2 (nth (+ i 1) Lista_blocos))
        	(setq aux2 (list (nth 0 Ponto2) (nth 1 Ponto2) 0))
        	(setq Dist (distof (rtos(distance  aux1 aux2)2 3) 3))
	        (setq aux1 (list (list Prog (* 10 (nth 2 Ponto1)) 0) (nth 3 Ponto1) ));;inicializa Perfil com progressiva 0.0 quando entra no While na 1 vez
	        (setq Perfil (append Perfil (list aux1)))
	        (setq Prog (+ Prog Dist))
	        (if (> (nth 2 Ponto1) Maior) (setq Maior (nth 2 Ponto1)))
	        (if (< (nth 2 Ponto1) Menor) (setq Menor (nth 2 Ponto1)))
        	(setq DN (distof (rtos (- (nth 2 Ponto2) (nth 2 Ponto1)) 2 3) 3))
        	(setq Ponto1 (append Ponto1 (list Dist DN Prog)))
        	(setq Lista_dados (append Lista_dados (list Ponto1)))
	        ;;escreve no arquivo externo a linha contendo Ponto, Desc, H e Progressiva
	        (setq Linha (strcat (rtos (nth 0 Ponto1 )2 3) ";"(rtos (nth 1 Ponto1 )2 3) ";" (nth 3 Ponto1 ) ";" (nth 4 Ponto1 ) ";" (rtos (nth 2 Ponto1 )2 3) ";"  (rtos Prog 2 3)))
	        (write-line Linha Log_File)
        	(setq i (+ i 1))
 	);;fim do while
	(if (> (nth 2 Ponto2) Maior) (setq Maior (nth 2 Ponto2)))
	(if (< (nth 2 Ponto2) Menor) (setq Menor (nth 2 Ponto2)))
        (setq Delta (* (- Maior Menor) 10.0 ))
 	(setq Ponto1 (nth (- (length Lista_blocos) 1) Lista_blocos))
 	(setq Ponto1 (append Ponto1 (list 0 0)))
	(setq Linha (strcat (nth 3 Ponto1 ) ";" (nth 4 Ponto1 ) ";" (rtos (nth 2 Ponto1 )2 3) ";"  ""))
	(write-line Linha Log_File)
  	(setq aux1 (list (list Prog (* 10 (nth 2 Ponto1)) 0) (nth 3 Ponto1) ));;Coloca o último ponto na lista
	(setq Perfil (append Perfil (list aux1)))
        (close Log_File)
	(princ "\nAquivo exportado")
 	(setq Lista_dados (append Lista_dados (list Ponto1)))
        (command "_.zoom" "E" "" )
 	(setq Ponto1 (getpoint "\nClique no ponto de inserção do perfil: " ))
        (setq Ht (getvar "TEXTSIZE"))
        (cabecalho Ponto1 Ht Prog)
        (Desenha_eixo Perfil Ponto1 Delta)
        (Coloca_Texto Perfil Ponto1) 
	(setvar "cmdecho" olcmdecho)
	(setvar "osmode" oldosmode)
	(setvar "pdmode" oldpdmode)
        (command "_.zoom" "E" "" )
)
(princ "\n")
(prompt "Digite 'Perf' para exportação de vértices a longo da polyline")
