(ql:quickload :qtools)
(ql:quickload :cells)
(ql:quickload :qtcore)
(ql:quickload :qtgui)
(ql:quickload :parse-number)
(ql:quickload :alexandria)
(ql:quickload :swank)

(sb-ext:save-lisp-and-die "qtools-repl" :executable t)
