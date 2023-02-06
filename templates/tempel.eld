;                       ..
;                      .//,
; .;///,               ,///'
; ;/////      .;       ,/;;;
; ,/.'//.    .//,      ,/; /,
;  ;  //,   .////.     ,//.';
;  .  ;//  .//'//;     ;//  ;
;     ,///;//' ;//;'  .//;  ..
;     .////;.  ./////;///'
;      ,;;,     .///////;
;                 ;///;'
;                   .
;
; Walheimat's tempel templates

prog-mode

(fixme (c "FIXME: "))
(todo (c "TODO: "))
(walheimat
 (c "                       ..") n
 (c "                      .//,") n
 (c " .;///,               ,///'") n
 (c " ;/////      .;       ,/;;;") n
 (c " ,/.'//.    .//,      ,/; /,") n
 (c "  ;  //,   .////.     ,//.';") n
 (c "  .  ;//  .//'//;     ;//  ;") n
 (c "     ,///;//' ;//;'  .//;  ..") n
 (c "     .////;.  ./////;///'") n
 (c "      ,;;,     .///////;") n
 (c "                 ;///;'") n
 (c "                   .") n
 (c "") n
 (c " ") p)

org-mode

(habit
 "* TODO " p n
 "SCHEDULED: " (substring (format-time-string (org-time-stamp-format) (current-time)) 0 -1) " .+" p "/" p ">" n
  ":PROPERTIES:" n
 ":STYLE: habit" n
 ":END:" n n)

(source
 "#+BEGIN_SRC " p n q n
 "#+END_SRC" :post (org-edit-src-code))

(quote
 "#+BEGIN_QUOTE" n
 q n
 "#+END_QUOTE")

conf-mode

(box
 "#" (make-string (+ 2 (length title)) ?#) "#" n
 "# " (s title) " #" n
 "#" (make-string (+ 2 (length title)) ?#) "#")

emacs-lisp-mode

(package
 ";;; " (s name) ".el --- " (s descr) " -*- lexical-binding: t; -*-" n n
 ";;; Commentary:" n
 ";;" n
 ";; " (s comm) n n
 ";;; Code:" n n
 q n n
 ";;; " name ".el ends here")

js-mode

(docstring
 "/**" n
 " * " q > n
 " */" >)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
