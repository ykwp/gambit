(define (php-generator msg . args)

  (define (entry-point name) (gen "run('" name "');\n?>\n"))
  (define (reg num) (gen "$reg[" num "]"))
  (define (stk num) (gen "$stack[$sp+" num "]"))
  (define (glo name) (gen "$glo[" (object->string
                                  (symbol->string name)) "]"))
  (define (clo lval index) (gen lval "[" index "]"))
  (define (lbl name) (gen "'" name "'"))
  (define (adjust-sp offset) (gen "$sp += " offset ";\n"))
  (define (void) (gen "UNDEFINED"))
  (define (proc-obj proc) (gen "'" proc "'"))
  (define (label-start name)
    (gen
     "\nfunction " name "() {\n"
     "global $glo;\n"
     "global $reg;\n"
     "global $stack;\n"
     "global $sp;\n"
     "global $nargs;\n"
     ))
  (define (label-stop) "}\n")
  (define (var-name name) (gen "$" name))

  (let ((fn (case msg
              ((entry-point) entry-point)
              ((reg) reg)
              ((stk) stk)
              ((glo) glo)
              ((clo) clo)
              ((lbl) lbl)
              ((adjust-sp) adjust-sp)
              ((void) void)
              ((proc-obj) proc-obj)
              ((label-start) label-start)
              ((label-stop) label-stop)
              ((var-name) var-name)
              (else
               (compiler-internal-error "unknown message" msg)))))
    (apply fn args)))

(define php-runtime
#<<EOF
<?php
$glo = array();
$reg = array(NULL);
$stack = array();
$sp = -1;
$nargs = 0;

function lbl1_fx_3c_() { // fx<
global $reg;
global $stack;
global $sp;
global $nargs;
if ($nargs !== 2) die("wrong number of arguments");
$reg[1] = $reg[1] < $reg[2];
return $reg[0];
};
$glo["fx<"] = 'lbl1_fx_3c_';

function lbl1_fx_2b_() { // fx+
global $reg;
global $stack;
global $sp;
global $nargs;
if ($nargs !== 2) die("wrong number of arguments");
$reg[1] = $reg[1] + $reg[2];
return $reg[0];
};
$glo["fx+"] = 'lbl1_fx_2b_';

function lbl1_fx_2d_() {
global $reg;
global $stack;
global $sp;
global $nargs;
if ($nargs !== 2) die("wrong number of arguments");
$reg[1] = $reg[1] - $reg[2];
return $reg[0];
};
$glo["fx-"] = 'lbl1_fx_2d_';

function lbl1_print() {
global $reg;
global $stack;
global $sp;
global $nargs;
if ($nargs !== 1) die("wrong number of arguments");
echo $reg[1] . "\n";
return $reg[0];
};
$glo["print"] = 'lbl1_print';

function run($pc) {
while ($pc !== NULL) $pc = $pc();
}
EOF
)
