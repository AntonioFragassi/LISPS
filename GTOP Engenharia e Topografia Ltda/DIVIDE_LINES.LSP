(defun c:DIVIDE_LINE ()
   (setq olcmdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (setq oldosmode (getvar "osmode"))
   (setvar "osmode" 0)
   (Princ "\n Selecione as curvas a CRIAR N�S: " )
   (if (setq  Conjunto (ssget))
    (progn
        (setq Comp_Conj (sslength Conjunto))
        (while (> Comp_Conj 0)
	        (setq Comp_Conj (1- Comp_Conj))
	        (setq elemento (ssname Conjunto Comp_Conj))
                (command "_.list" elemento "")
                (print (setq per (getvar "perimeter")))	
                (setq nparte (fix (/ (getvar "perimeter") 2)))
                (if (> nparte 1) (command "_.DIVIDE" elemento nparte ""))
        )
    )
  )
 (setvar "cmdecho" olcmdecho)
 (setvar "osmode" oldosmode)
)
(princ "\n")
(prompt "Digite 'DIVIDE_LINE' para criar v�rtices a longo da polyline")












