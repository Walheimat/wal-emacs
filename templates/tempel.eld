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
