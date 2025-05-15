(defun c:3DTo2D (/ ent enttype lst num-vertices param pt pt2D pl)
  (setq ent (car (entsel "\nSelecione a 3D Polyline: "))) ; Seleciona a entidade 3D Polyline
  (setq enttype (cdr (assoc 0 (entget ent)))) ; Obtém o tipo da entidade

  (if (equal enttype "POLYLINE") ; Verifica se a entidade é uma Polyline 3D
    (progn
      (setq lst '()) ; Inicializa a lista de pontos 2D
      (setq num-vertices (1+ (vlax-curve-getEndParam ent))) ; Obtém o número total de vértices
      
      (setq param 0) ; Inicializa o parâmetro para os vértices
      (while (< param num-vertices)
        (setq pt (vlax-curve-getPointAtParam ent param)) ; Obtém o ponto na 3D Polyline
        (setq pt2D (list (car pt) (cadr pt))) ; Converte para 2D (X e Y)
        (setq lst (cons pt2D lst)) ; Armazena o ponto 2D na lista
        (setq param (1+ param))) ; Incrementa o parâmetro

      (setq lst (reverse lst)) ; Reverte a lista para manter a ordem correta

      ; Cria uma nova Polyline 2D
      (setq pl (entmakex (append
                           (list '(0 . "LWPOLYLINE") ; Tipo da entidade
                                 '(100 . "AcDbEntity") 
                                 '(100 . "AcDbPolyline")
                                 (cons 90 (length lst)) ; Número de vértices
                                 '(70 . 0)) ; Fechamento (0 = aberto, 1 = fechado)
                           (mapcar '(lambda (pt) (cons 10 pt)) lst)))) ; Adiciona os vértices 2D

      (princ "\n3D Polyline convertida para Polyline 2D.")
    )
    (princ "\nA entidade selecionada não é uma Polyline 3D.") ; Caso a entidade selecionada não seja válida
  )
  (princ)
)
