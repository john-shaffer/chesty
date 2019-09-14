(ns chesty.db)

(defn id [doc-or-id]
  (if (string? doc-or-id)
    doc-or-id
    (:_id doc-or-id (:id doc-or-id))))
