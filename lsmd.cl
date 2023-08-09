#!/usr/bin/env -S sbcl --script

(load "~/.local/share/common-lisp/quicklisp/setup.lisp")
(ql:quickload "uiop")
(ql:quickload "bordeaux-threads")
(ql:quickload "usocket")

(defconstant *unix-epoch-offset*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defconstant *current-millennium-AD* 2000)

(defun universal-to-unix-time(universal-time)
  (- universal-time *unix-epoch-offset*)
  )

(defun get-unix-time()
  (universal-to-unix-time (get-universal-time))
  )

(defun unix-to-universal-time(unix-time)
  (+ unix-time *unix-epoch-offset*)
  )

(defun unix-to-timedate(unix-time)
  (multiple-value-bind (seconds minutes hours day month year) (decode-universal-time (unix-to-universal-time unix-time))
    (format nil "~D/~D/~D--~D:~D.~D" month day (- year *current-millennium-AD*) hours minutes seconds))
  )

(defun err(errno &optional str)
  (case errno
    (1 (format t "lsmd: Failed to open configuration file: ~A. Do I have correct permissions?" str))
    ) ;some other possible errs:
  	;failed to bind to socket
	;invalid/illegal config
	;
  (sb-ext:exit :code errno)
  )

(defun neterr(errno &optional command args)
    (concatenate 'string (write-to-string errno) "@"
		 (case errno
		   (11 (format nil "Unrecognized command ~A:~A" command args))
		   (12 (format nil "Cannot ~A the program ~A (not defined in configuration)" command args))
		   (13 (format nil "Process ~A failed to start." command))
		   (14 (format nil "Cannot ~A the program ~A (not in configuration)" command args))
		   (15 (format nil "Cannot stop the program ~A (not in environment)" args))
		   (16 (format nil "Cannot start the program ~A (program already running)" args))
		   (17 (format nil "Cannot stop the program ~A (program not started)" args))
		   (99 (format nil "Unknown error"))
      ))
  )

(defun cfg-buildline(pline nline)
  (if nline
    ;TODO: Proper validation of config values here.  Err if bad
    ; this should be a proper wrapper and not just 2 lines.
    (append pline nline))
  )

(defun select-db(db label value)
  (car (remove-if-not #'(lambda (dbline) (equal (getf dbline label) value)) db))
  )

(defun select-dbp(db progr label)
  (getf (select-db db :prog progr) label)
  )

(defun delete-db(db label value)
  (remove-if #'(lambda (dbline) (equal (getf dbline label) value)) db)
  )

(defun configure(file)
  (let ((cfgfile (open file :if-does-not-exist nil)))
    (cond ((not cfgfile)
	   (return-from configure (err 1 file))))
    (loop with progline with progs
	  do (multiple-value-bind (line eof) (read-line cfgfile nil)
	       (cond
		 (eof ;TODO: Allowances for whitespace in config here
		   (push progline progs)
		   (close cfgfile)
		   (return-from configure progs))
		 ((eq (char line 0) #\[)
		  (cond (progline
			  (push progline progs)))
		  (setq progline (cfg-buildline NIL (list :prog (subseq line 1 (position #\] line :test #'equal))))))
		 (t
		   (let ((setting (uiop:split-string line :separator "=")))
		     (setq progline (cfg-buildline progline (list (intern (string-upcase (car setting)) "KEYWORD")
								  (car (last setting))))))))))) ;TODO: A more efficient way than breaking the string down and making it again?
  )

(defun start(progs env progr &optional automatic)
  (let ((progline (select-db progs :prog progr))
	(envline (select-db env :prog progr)))

    (cond ((not progline)
	   (return-from start (neterr 12 "start" progr)))
	  (envline
	    (cond ((uiop:process-alive-p (getf envline :nfo))
		   (return-from start (neterr 16 "start" progr)))
		  (t
		    (setf env (delete-db env :prog progr))))))

    (loop for req in (uiop:split-string (getf progline :require))
	  do (let ((nfo (select-dbp env req :nfo)))
	       (cond (nfo
		       (cond ((uiop:process-alive-p nfo)
			      nil)
			     (t
			       (multiple-value-bind (rets nenv) (start progs env req)
				 (cond ((not (string= "0" rets))
					(return-from start rets)))
				 (setf env nenv)))))
		     (t
		       (multiple-value-bind (rets nenv) (start progs env req)
			 (cond ((not (string= "0" rets))
				(return-from start rets)))
			 (setf env nenv))))))

    (let ((nfo (uiop:launch-program (getf progline :start)
				    :error-output (or (getf progline :stderr) (concatenate 'string (uiop:getenv "HOME") "/.var/log/" progr ".log"))
				    :if-error-output-exists :append
				    :directory (select-dbp progs progr :dir))))
      (bt:make-thread
	(lambda ()
	  (let ((exit-code (uiop:wait-process nfo)))
	    (cond ((and (plusp exit-code) (not automatic))
		   (start progs env progr t)))) ;TODO: Mark & notify user when auto restart fails, rather than doing nothing. Maybe a config option? 
	  )
	:name (concatenate 'string progr "on lsm"))

      (cond ((getf progline :secs)
	     (sleep (parse-integer *)) ;Should work here I think. also remember to do config checking rather than parsing the int here.
	     (cond ((plusp (slot-value nfo 'uiop/launch-program::exit-code))
		    (return-from start (neterr 13 progr)))))) ;TODO: Turn this into an AND or something cuz it's ugly

      (values "0" (push (list :prog progr :nfo nfo :time (get-unix-time) :ll 0) env))))
  )

(defun stop(progs env progr)
  (let ((progline (select-db progs :prog progr))
	(nfo (select-dbp env progr :nfo)))
    (cond ((not progline)
	   (return-from stop (neterr 14 "stop" progr)))
	  ((not nfo)
	   (return-from stop (neterr 15 "stop" progr)))
	  ((not (uiop:process-alive-p nfo))
	   (return-from stop (neterr 17 "stop" progr)))) ;TODO: make this only send progr [keywords maybe?] - also, might want better control flow here.
    (let ((stop (select-dbp progs progr :stop)))
      (cond (stop
	      (uiop:run-program stop))
	    (t
	      (uiop:run-program (format nil "pkill -P ~D" (uiop:process-info-pid nfo)))))))  ;TODO: add killsecs attribute.  Try kill with SIGTERM & if killsecs not met, then use SIGKILL.
    ;Also need an if here that runs stop script rather than killing if specified.
    "0"
  )

(defun statusline(progline env &optional notime)
  (handler-case ;TODO: I don't really need the error handler, can just do (cond nfo) and then try to read slot.
    (let* ((envline (select-db env :prog (getf progline :prog)))
	   (nfo (getf envline :nfo))
	   (alive (uiop:process-alive-p nfo)))
      (let ((esc (code-char 27)))
	(concatenate 'string
		     (cond (alive
			     (format nil "~C[92mrunning~C[0m" esc esc))
			   (t (let ((exitcode (slot-value nfo 'uiop/launch-program::exit-code)))
				(cond ((plusp exitcode)
				       (format nil "~C[31mdead~C[0m" esc esc))
				      ((zerop exitcode)
				       (format nil "~C[32mfinished~C[0m" esc esc))
				      (t "not started")))))
		     (let ((launchtime (getf envline :time))
			   (timeformat (getf progline :time)))
		       (cond ((and (not notime) launchtime)
			      (format nil "; started at ~D" (cond ((string= "unix" timeformat)
								   (launchtime))
								  (t 
								    (unix-to-timedate launchtime)))))
			     (t nil))))))
      (sb-pcl::missing-slot (c) "not started"))
  )

(defun status(progs env &optional progr) 
  (cond ((> (length progr) 0)
	 (let ((progline (select-db progs :prog progr)))
	   (cond ((not progline)
		  (return-from status (neterr 12 "status" progr))))
	   (concatenate 'string
			(handler-case
			  (format nil "0@\~A:~%~Cstatus: ~A~%~Cstart: ~A~%~Cprocess: ~D~%~%" progr #\tab (statusline progline env) #\tab (getf progline :start) #\tab (uiop:process-info-pid (select-dbp env progr :nfo)))
			  (sb-pcl::missing-slot (c) (format nil "0@\~A:~%~Cstatus: ~A~%~Cstart: ~A~%~Cprocess: NIL~%~%" progr #\tab (statusline progline env) #\tab (getf progline :start) #\tab)))
			(let ((output (make-string-output-stream)))
			  (uiop:run-program (format nil "tail -n 5 ~A" (or (getf progline :stderr) (concatenate 'string (uiop:getenv "HOME") "/.var/log/" progr ".log"))) :output output) ;TODO: unsafe
			  (get-output-stream-string output))))
	 )
	(t
	  (let ((globalstatus "0@"))
	    (loop for progline in (delete-db progs :prog "lsm") 
		  do (let ((progr (getf progline :prog))
			   (status (statusline progline env t)))
		       (setq globalstatus (concatenate 'string globalstatus
						       (format nil "~A:~A~C~A~C~%"
							       progr
							       (format nil "~v{~a~:*~}" (- 79 (+ (+ (length progr) 1) (+ (length status) 
															 (cond ((find #\m status)
																-7)
															       (t 2))
																      ))) '(#\space))
							       #\[
							       status
							       #\])))))
	    globalstatus)))
  )

(defun run_command(command &optional args &key progs env) ;TODO: Shutdown all command to exit session gracefully
  (cond ((string= command "start")
	 (start progs env args))
	((string= command "stop")
	 (values (stop progs env args) env))
	((string= command "restart")
	 (stop progs env args)
	 (start progs env args))
	((string= command "status")
	 (values (status progs env args) env))
	(t (neterr 11 command args)))
  )

(defun str-cmd(str)
  (let ((strl (uiop:split-string str :separator " ")))
    (values (pop strl) (pop strl)))
  )

(defun handle-request(stream progs env)
  (let ((line (read-line stream)))
    (multiple-value-bind (rets nenv) (multiple-value-call #'run_command (str-cmd line) :progs progs :env env)
      (format stream "~a" rets)
      (terpri stream)
      (force-output stream)
      nenv))
  )

;TODO: finish calling command evaluator NOTE: now this changed, what about sanitizing and error-checking input?
(defun server_listen(addr port progs env)
  (let ((socket (usocket:socket-listen addr port :reuse-address t)))
    (usocket:wait-for-input socket)
    (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
      (let ((nenv (handle-request stream progs env)))
	(close stream)
	(usocket:socket-close socket)
	nenv)))
  )  

(defun run()
  (let ((cfgloc)
	(args (uiop:command-line-arguments)))
    (if args
      (setq cfgloc (pop args))
      (setq cfgloc (concatenate 'string (uiop:getenv "HOME") "/.config/lsmrc")))
    (loop with progs = (configure cfgloc) with env
	    do (setf env (server_listen (select-dbp progs "lsm" :addr) (parse-integer (select-dbp progs "lsm" :port)) progs env))))
  )

(run)
