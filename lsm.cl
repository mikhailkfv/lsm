#!/usr/bin/env -S sbcl --script

(load "~/.local/share/common-lisp/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "usocket")

(defun err(errno &optional str)
  (case errno
    (1 (format t "lsm: Unknown argument ~A.~%Try \"lsm help\" for commands.~%" str))
    (2 (format t "lsm: lsmd returned error: ~&~A~%" str))
    (3 (format t "lsm: Unable to connect to lsmd: ~A~%" str))
    )
  (sb-ext:exit :code errno)
  )

(defun help()
  (format t "Usage: lsm <directive> [service]
Start, stop, and manage daemons as <service> monitored by lsm.
Valid directives:
   help			 show this help page.
   start <service>	 start a service.
   stop <service>	 stop a service.
   restart <service>	 restart a service.
   status [service]	 show service status & log, or general session status.~%")
  )

(defun talk(addr port message)
  (let* ((socket (usocket:socket-connect addr port))
	 (stream (usocket:socket-stream socket)))
    (write-line message stream)
    (force-output stream)
    (loop for input = (read-line stream nil :eof)
	  until (eq input :eof)
	  do 
	  (cond ((string= "0" input)
		 nil)
		((find #\@ input)
		 (let* ((result (uiop:split-string input :separator "@"))
			(retn (parse-integer (pop result)))
			(rets (pop result)))
		   (cond ((plusp retn)
			  (err 2 rets)))
		   (cond (rets
			   (format t rets)
			   (terpri)))))
		(t
		  (format t input)
		  (terpri))))
    (close stream)
    (usocket:socket-close socket))
  )

(defun run()
  (let* ((args (uiop:command-line-arguments))
	 (directive (pop args)))
    (cond ((string= directive "help")
	   (help))
	  ((or (string= directive "start")
	       (string= directive "stop")
	       (string= directive "restart")
	       (string= directive "status"))
	   (handler-case
	     (talk "127.0.0.1" 42024 (concatenate 'string directive " " (pop args)))
	     (usocket:connection-refused-error (c)
					       (err 3 (write-to-string c)))))
	  (t (err 1 directive))))
  )
