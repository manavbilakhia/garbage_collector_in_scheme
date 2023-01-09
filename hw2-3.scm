;;1
<binary-string>::= 0 <binary-string>
                 | 1 <binary-string>
                 | 0
                 | 1
                 | 01 <binary-string>
                 | 00 <binary-string>
                 | 11 <binary-string>
                 | 10 <binary-string>3
;;2
<binary-string>::= 0 <binary-string>
                 | 1 <binary-string>
                 | 0
                 | 1
;;3 
<palindrome-binary-string>::= 0 <binary-string> 0
                            | 1 <binary-string> 1
                            | 0
                            | 1
                            | ε
;;4
<hex>::= <poshex>
       | <neghex>
<poshex>::= <int> <poshex>
           |<string> <poshex>
           |<int>
           |<string>
<neghex>::= -<int> <poshex>
           |-<string> <poshex>
           |-<int>
           |-<string>
<int>::= 0
        |1
        |2
        |3
        |4
        |5
        |6
        |7
        |8
        |9
<string>::= a
           |b
           |c
           |d
           |e
           |f
;;5
<py-function-signature>::= "def" <ID> "("<args>"):"
<args>::= <ID> = <exp>, <args>
        | <ID>, <args>
        | <ID> = <exp>
        | <ID>
;; assuming that production rules for <ID> and <exp> are predefined