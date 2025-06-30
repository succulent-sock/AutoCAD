(defun c:SetOneAttrWidth ( / blkName tagName newWidth ss i blkEnt attEnt attData)
  ;; Hardcoded values
  (setq blkName "Revision Title Block")
  (setq tagName "DATE2")
  (setq newWidth 0.75)

  ;; Select all INSERT entities with the given block name
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blkName))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq blkEnt (ssname ss i))
        ;; Start with the first sub-entity of the block
        (setq attEnt (entnext blkEnt))
        (while (and attEnt (/= (cdr (assoc 0 (entget attEnt))) "SEQEND"))
          (setq attData (entget attEnt))
          (if (and (= (cdr (assoc 0 attData)) "ATTRIB")
                   (= (strcase (cdr (assoc 2 attData))) (strcase tagName)))
            (progn
              (setq attData (subst (cons 41 newWidth) (assoc 41 attData) attData)) ; Set width factor
              (entmod attData)
              (entupd blkEnt)
            )
          )
          (setq attEnt (entnext attEnt)) ; Move to next entity in block
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nUpdated width factor to " (rtos newWidth 2 2)
                     " for tag \"" tagName "\" in block \"" blkName "\"."))
    )
    (princ "\nNo matching blocks found.")
  )
  (princ)
)