(defun c:UpdateRevisionBlock ( / ss i ent obj revVal revNum attDict atts att tag revAtt)
  (setq ss (ssget "_X" '((0 . "INSERT")))) ; Get all block inserts

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (if (vlax-property-available-p obj 'HasAttributes)
          (progn
            (setq atts (vlax-invoke obj 'GetAttributes))
            (setq attDict '())
            (setq revVal nil)
            (setq revNum nil)

            ;; Build an assoc list: (("TAG" . <attribute object>) ...)
            (foreach att atts
              (setq tag (strcase (vla-get-tagstring att)))
              (setq attDict (cons (cons tag att) attDict))
              (if (= tag "REVISION0")
                (setq revVal (vla-get-textstring att))
              )
            )

            ;; Step 1: Update REVISION0 value
            (cond
              ((= revVal "B")
               (setq revVal "C")
               (setq revAtt (cdr (assoc "REVISION0" attDict)))
               (if revAtt (vla-put-textstring revAtt revVal))
              )
              ((= revVal "A")
               (setq revVal "B")
               (setq revAtt (cdr (assoc "REVISION0" attDict)))
               (if revAtt (vla-put-textstring revAtt revVal))
              )
            )

            ;; Step 2: Convert updated Revision Version to a number
            (setq revNum
              (cond
                ((= revVal "A") 1)
                ((= revVal "B") 2)
                ((= revVal "C") 3)
                ((= revVal "D") 4)
                (T 0) ; default or unknown
              )
            )

            ;; Step 3: Dynamically build tag names and update values
            (if (> revNum 0)
              (progn
                (setq
                  revTag (strcat "ROW_" (itoa revNum) "_REV.-DESCRIPTION")
                  descTag (strcat "DESCRIPTION_ROW_" (itoa revNum) "_REV")
                  dateTag (strcat "DATE_ROW_" (itoa revNum) "_REV")
                  approvedTag (strcat "APPROVED_ROW_" (itoa revNum) "_REV")
                )

                ;; Step 4: Update the attributes if they exist
                (if (cdr (assoc revTag attDict))
                  (vla-put-textstring (cdr (assoc revTag attDict)) revVal)
                )
                (if (cdr (assoc descTag attDict))
                  (vla-put-textstring (cdr (assoc descTag attDict)) "NEW ROW")
                )
                (if (cdr (assoc dateTag attDict))
                  (vla-put-textstring (cdr (assoc dateTag attDict)) "20250516")
                )
                (if (cdr (assoc approvedTag attDict))
                  (vla-put-textstring (cdr (assoc approvedTag attDict)) "Hannah Layton")
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (princ)
)
