	;;ROTINA PARA DESENHAR TALUDES
	
	;;Julho / 2000
	;;By José Sagi Neto

(defun c:talude  ()

(defun *error* (errmsg)

   (alert "\nPrograma abortado !!!") (terpri)
   (prompt (strcat "O motivo foi * " errmsg " *"))
   (princ)

 )
      (setvar "cmdecho" 0)
       (setvar "pdmode" 1)
        (command "layer" "m" "talude" "c" "8" "talude" "")  ;;Cria o Layer Talude na cor 8 Cinza
        (setq crista (entsel "\nSelecione a crista : "))
        (setq w 1)
        (while w
          (if (not crista)
             (setq crista (entsel "\nSelecao vazia. Selecione a crista : "))
             (setq w nil)
          )
        )
        (if (/= (cdr (assoc 0 (entget (car crista)))) "LWPOLYLINE")
           (progn
              (alert "Não é uma Polyline, Por Favor Utilize o Comando PEdit")
              (quit)
           )
        )
        (command "chprop" crista "" "c" "3" "lt" "continuous" "")  ;;Altera a cor da polyline selecionada (Crista)
        (setq pe (entsel "\nSelecione o pe: "))
        (setq w 1)
        (while w
          (if (not pe)
             (setq pe (entsel "\nSelecao vazia. Selecione o pe : "))
             (setq w nil)
          )
        )
        (if (/= (cdr (assoc 0 (entget (car pe)))) "LWPOLYLINE")
           (progn
              (alert "Não é uma Polyline, Por Favor Utilize o Comando PEdit")
              (quit)
           )
        )
        (command "chprop" pe "" "c" "4" "lt" "continuous" "")  ;;Altera a cor da polyline selecionada (Pé)
        (setq dist (getdist "\nIndique  o espacamento <2.00>: "))
        (if (not dist)
           (setq dist 2.00)
        )
        (command "area" "e" crista)
        (setq lcrista (getvar "perimeter"))
        (setq nsurf (fix (/ lcrista dist)))
        (command "surftab1" nsurf)
        (command "rulesurf" (nth 1 crista) (nth 1 pe))
        (setq malha (entlast))
        (setq vt1 malha)
        (setq vt2 malha)
        (setq sel1 (ssadd))

        (repeat (+ 1 nsurf)
          (setq vt2 (entnext vt2))
        )
        (setq cont 1.0)
        (repeat (+ 1 nsurf)
          (setq vt1 (entnext vt1))
          (setq vt2 (entnext vt2))
          (command "line" (cdr (assoc 10 (entget vt1))) (cdr (assoc 10 (entget vt2))) "")
          (if (= cont -1.0)
             (command "scale" (entlast) "" (cdr (assoc 10 (entget vt1))) "0.35")
          )
          (setq cont (* cont -1.0))
        )
        (command "erase" malha "")
        (command "chprop" crista "" "c" "BYL" "LA" "TALUDE" "")  ;;Altera a crista para layer talude
        (command "chprop" pe "" "c" "BYL" "LA" "TALUDE" "")  ;; Altera o pé para layer talude
        (command "layer" "s" "0" "")  ;;
        (command "redraw")
        (princ)
     )
