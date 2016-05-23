(ql:quickload :elephant)
(ql:quickload :clsql)

(use-package :elephant)

(open-store '(:clsql (:sqlite "test-db/sqlite.db")))
