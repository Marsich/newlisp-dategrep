#!/usr/local/bin/newlisp

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org>

(constant '*VERSION* "0.6")

(module "getopts.lsp")


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Define all possibile valid patterns for start-date & end-date.
;;
;- - - Test YYYY-MM-DD HH:MM:SS 
(define (pattern-date-1 date-to-test)
	(regex "^([0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])\\s(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])$" date-to-test)	)

;- - - Test YYYY-MM-DD HH:MM
(define (pattern-date-2 date-to-test)
	(regex "^([0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])\\s(2[0-3]|[01][0-9]):([0-5][0-9])$" date-to-test))

;- - - Test YYYY-MM-DD
(define (pattern-date-3 date-to-test)
	(regex "^([0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])$" date-to-test))

;- - - Test HH:MM:SS 
(define (pattern-date-4 date-to-test)
	(regex "^(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])$" date-to-test))

;- - - Test HH:MM
(define (pattern-date-5 date-to-test)
	(regex "^(2[0-3]|[01][0-9]):([0-5][0-9])$" date-to-test))
		

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; This procedure builds the start date, based on the patterns described above.
;; It works changing the elements of a EPOCH date (1970 1 1 0 0 0) with those supplied
;; by the user.
;; If only the time part is given, the procedure "fills" the missing elements
;; (Year Month & Day) with the current date (NOW).
;;
(define (build-start-date my-date)
	(setf my-date-list EPOCH)
	(setf my-date (trim my-date))
	
	(cond 
	;
	((setf x (pattern-date-1 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10))
		(setf (my-date-list 3) (int (x 12 0 10)))
		(setf (my-date-list 4) (int (x 15) 0 10))
		(setf (my-date-list 5) (int (x 18) 0 10)))
	;
	((setf x (pattern-date-2 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10))
		(setf (my-date-list 3) (int (x 12) 0 10))
		(setf (my-date-list 4) (int (x 15) 0 10)))
	;
	((setf x (pattern-date-3 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10)))
	;
	((setf x (pattern-date-4 my-date))
		(setf my-date-list NOW)
		(setf (my-date-list 3) (int (x 3) 0 10))
		(setf (my-date-list 4) (int (x 6) 0 10))
		(setf (my-date-list 5) (int (x 9) 0 10)))
	;
	((setf x (pattern-date-5 my-date))
		(setf my-date-list NOW)
		(setf (my-date-list 3) (int (x 3) 0 10))
		(setf (my-date-list 4) (int (x 6) 0 10))
		(setf (my-date-list 5) 0))
	;
	(true my-date-list)	
	)	
	(date-value my-date-list)
)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; This procedure builds the end date, based on the patterns described above.
;; It works changing the elements of a NOW date (ie 2017 1 4 16 21 12) with those supplied
;; by the user.
;;
(define (build-end-date my-date)
	(setf my-date-list NOW)
	(setf my-date (trim my-date))
	
	(cond 
	;
	((setf x (pattern-date-1 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10))
		(setf (my-date-list 3) (int (x 12) 0 10))
		(setf (my-date-list 4) (int (x 15) 0 10))
		(setf (my-date-list 5) (int (x 18) 0 10)))
	;
	((setf x (pattern-date-2 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10))
		(setf (my-date-list 3) (int (x 12) 0 10))
		(setf (my-date-list 4) (int (x 15) 0 10))
		(setf (my-date-list 5) 59))
	;
	((setf x (pattern-date-3 my-date))
		(setf (my-date-list 0) (int (x 3) 0 10))
		(setf (my-date-list 1) (int (x 6) 0 10))
		(setf (my-date-list 2) (int (x 9) 0 10))
		(setf (my-date-list 3) 23)
		(setf (my-date-list 4) 59)
		(setf (my-date-list 5) 59))
	;
	((setf x (pattern-date-4 my-date))
		(setf (my-date-list 3) (int (x 3) 0 10))
		(setf (my-date-list 4) (int (x 6) 0 10))
		(setf (my-date-list 5) (int (x 9) 0 10)))
	;
	((setf x (pattern-date-5 my-date))
		(setf (my-date-list 3) (int (x 3) 0 10))
		(setf (my-date-list 4) (int (x 6) 0 10))
		(setf (my-date-list 5) 59))
	;
	(true my-date-list)	
	)	
	(date-value my-date-list)
)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; This procedure converts the placeholders in the format string into the corresponding
;; regex expressions (see http://www.newlisp.org/downloads/manual_frame.html#date).
;; 
;; Please note that the differences between "%-d" and "%-D" 
;;
(define (date-regex-convert str)
	;- - - Year
	(setf str (replace "%y" str "[0-9]{2}"))
	(setf str (replace "%Y" str "(19|20)[0-9]{2}"))	
	
	;- - - Month
	(setf str (replace "%b" str "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"))
	(setf str (replace "%B" str "(January|February|March|April|May|June|July|August|September|October|November|December)"))
	(setf str (replace "%m" str "(0[1-9]|10|11|12)"))

	;- - - Day
	(setf str (replace "%d" str "(3[01]|[0]?[1-9]|[12][0-9])"))
	;(setf str (replace "%-d" str "(3[01]|[0]?[1-9]|[12][0-9])")) ; 0 is optional
	;(setf str (replace "%-D" str "(3[01]|(\\\\s[1-9])|[12][0-9])")) ; 0 or space must exist	
	
	;- - - Hour
	(setf str (replace "%H" str "(2[0-3]|[01][0-9])"))	; 24h
	(setf str (replace "%I" str "(0[1-9]|10|11|12)"))	; 12h
	
	;- - - Minutes
	(setf str (replace "%M" str "([0-5][0-9])"))
	
	;- - - Seconds
	(setf str (replace "%S" str "([0-5][0-9])"))

	;- - - Am/Pm
	(setf str (replace "%p" str "(am|pm|AM|PM|Am|Pm)"))
	
	(setf str (string "(" str ")")) ; to have a $1 surrounding regex group
)



;- - - Find my current timezone
(define (my-time-zone) (now 0 -2))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Main begins here!
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



;- - - Define all possible options on the command line
(longopt "debug" (setf debug-mode true) nil "	Debug mode.")
;
(shortopt "f" (setf format-string getopts:arg) "<format>" "		Format date")
(longopt "format" (setf format-string getopts:arg) "<format>" "		Format date")
;
(shortopt "s" (setf start-date-string getopts:arg) "<date>" "		Starting date")
(longopt "start" (setf start-date-string getopts:arg) "<date>" "		Starting date")
(longopt "from" (setf start-date-string getopts:arg) "<date>" "		Starting date")
;
(shortopt "e" (setf end-date-string getopts:arg) "<date>" "		Ending date")
(longopt "end" (setf end-date-string getopts:arg) "<date>" "		Ending date")
(longopt "to" (setf end-date-string getopts:arg) "<date>" "		Ending date")
;
(shortopt "m" (setf multiline-mode true) nil "	Print all lines between the start and end line even if they are not timestamped.")
(longopt "multiline" (setf multiline-mode true) nil "	Print all lines between the start and end line even if they are not timestamped.")
;
(shortopt "k" (setf skip-unparsable true) nil "Ignore all lines without timestamp. Disables --multiline.")
(longopt "skip-unparsable" (setf skip-unparsable true) nil "Ignore all lines without timestamp. Disables --multiline.")
;
(shortopt "v" (setf display-version true) nil "	Version")
(longopt "version" (setf display-version true) nil "	Version")
;
(shortopt "h" (getopts:usage) nil "		Print this help message")
(longopt "help" (getopts:usage) nil "		Print this help message")


;- - - Parse command line
(setf extra-argv (getopts (2 (main-args))))


;- - - Show version and quit.
(if (true? display-version)
	(begin
		(println "dategrep (newLISP®) vers. " *VERSION*)
		(exit)
	)
)

;- - - Ooops we didn't define a filename.
(if (= extra-argv '() )
	(begin
		(println "Missing log file to grep!")
		(exit)
	)
)

;- - - OK, we have something *after* the options (we have a possible filename).
(setf log-file (extra-argv 0))

;- - - Check for existence of input file.
(if (not (file? log-file))
	(begin
		(println "Log file [" log-file "] not found!")
		(exit)
	)
)

;- - - Now we can set min e max date/time
(setf EPOCH '(1970 1 1 0 0 0))
(setf NOW (slice (now) 0 6))

;- - - Let's normalize start-date
(if (nil? start-date-string)
	(setf start-date 0)
	(setf start-date (build-start-date start-date-string))
)

;- - - Let's normalize end-date
(if (nil? end-date-string)
	(setf end-date (date-value NOW))
	(setf end-date (build-end-date end-date-string))
)


;- - - Let's define a format date to search in log file.
(if (nil? format-string)
	(begin
	(setf format-string  "%Y-%m-%d %H:%M:%S") ; this is the default format
	)
	(begin
		(cond
			((= format-string "apache") (setf format-string "%d/%b/%Y:%H:%M:%S"))
			((= format-string "postfix") (setf format-string "%b %d %H:%M:%S"))
			(true (setf format-string "%Y-%m-%d %H:%M:%S"))			
		)
	)
)


;- - - Build a regex date to search the log file.
(setf date-format (date-regex-convert format-string))


;- - - Check that a %Y (or %y) exists in format-string. If not, let's add it.
; Because some log files do not have the year (as Postfix mail.log 
; "Jan  4 09:57:55 ....") we must provide one to correctly check the star/end date.
; We'll add an "uncommon pattern" to the format provided by the user and set
; the variable "current-year-correction" to be used later.
;
(if (not (find "%Y" format-string 1))
	(begin
		(setf format-string (string "§§§%Y§§§ " format-string))
		(setf current-year-correction (string "§§§" (date-list (date-value (now)) 0) "§§§ "))
	)
	(setf current-year-correction nil)
)	

;- - - Let's display main parameters (--debug).
(if (true? debug-mode)
	(begin
		(println "\nDATEGREP PARAMETERS:")
		(println "--version         " *VERSION*)
		(println "--start           " start-date-string " --> " start-date " --> " (date start-date  (dec 0 (my-time-zone))))
		(println "--end             " end-date-string " --> " end-date " --> " (date end-date (dec 0 (my-time-zone))))
		(println "--format          " format-string " --> " date-format)
		(println "--multiline       " multiline-mode)
		(println "--skip-unparsable " skip-unparsable "\n")	
		(exit)
	)
)

(setf IN-RANGE nil) ;- - - When true means that we are in range [min-date] >> [max-date]
(setf $1 nil) ;- - - $1 was set to the last option of the command line.

;- - - OK! Ready to go!
(set 'fh (open log-file "read"))
(while (setf line (read-line fh))
	(regex date-format line)
	;(println date-format)
	;
	(cond
		((and (nil? $1) (true? skip-unparsable)) (println "-"))	
		;
		((and (nil? $1) (true? IN-RANGE) (true? multiline-mode))
			(println line))
		;
		((true? $1)
		;;(println "L: " line)
		;;(println "$1/1: " $1)
			(if (true? current-year-correction)
				(setf $1 (string current-year-correction $1))
			)
			;;(println "$1/2: " $1)
			;;(println "FS: " format-string)
			(setf D (date-parse $1 format-string))
			(println "D: " D)

			(cond
				((and (>= D start-date) (<= D end-date))
					(setf IN-RANGE true)
					(println line))
				;
				((> D end-date)
					;;(println "+EOF")
					(close fh)
					(exit))
				;
				(true (println "+ " line))
			)
		)
		;((nil? $1) (println "NIL " line))
	)
	(setf $1 nil)	
)   
;;(println "FINITO")
(close fh)
(exit)