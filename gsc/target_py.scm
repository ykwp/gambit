(define py2-generator
  (let ((indent 0))
    (define (py-indent) (set! indent (+ indent 1)) "")
    (define (py-dedent) (set! indent (- indent 1)) "")
    (define (py-newline) (gen "\n" (make-string (* 4 indent) #\space)))

    (define (entry-point name)
      (gen "run(" name ")" (py-newline)))

    (define (reg n) (gen "reg[" n "]"))
    (define (stk n) (gen "stack[sp+" n "]"))
    (define (glo name) (gen "glo['" name "']"))
    (define (clo lval index) (gen lval "[" index "]"))
    (define (lbl name) name)
    (define (adjust-sp offset)
      (gen "sp += " offset (py-newline)))
    (define (void) "None")
    (define (proc-obj p) p)
    (define (label-start name)
      (gen "def " name "():"
           (py-indent)
           (py-newline)
           "global glo" (py-newline)
           "global reg" (py-newline)
           "global stack" (py-newline)
           "global sp" (py-newline)
           "global nargs" (py-newline)))
    (define (label-stop)
      (py-dedent)
      (py-newline))
    (define (var-name name) name)
    (define (comment . xs) (gen "# " xs (py-newline)))
    (define (die msg)
      (gen "raise Exception('" msg "')" (py-newline)))
    (define (copy a b) (gen a " = " b (py-newline)))
    (define (apply_ a b) (gen a " = " b (py-newline)))
    (define (return expr) (gen "return " expr (py-newline)))
    (define (if_ cond_ then else_)
      (gen "if " cond_ ":"
           (py-indent)
           (py-newline)
           then
           (py-dedent)
           (py-newline)
           "else:"
           (py-indent)
           (py-newline)
           else_
           (py-dedent)
           (py-newline)))
    (define (narg-check n)
      (if_ (gen (var-name 'nargs) " != " n)
           (die "incorrect number of arguments")
           (gen "pass")))
    (define (equal a b)
      (gen a " == " b))
    (define (false) "False")



    (lambda (msg . args)
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
                  ((comment) comment)
                  ((narg-check) narg-check)
                  ((die) die)
                  ((copy) copy)
                  ((apply) apply_)
                  ((if) if_)
                  ((return) return)
                  ((false) false)
                  ((equal) equal)
                  (else
                   (compiler-internal-error "unknown message" msg)))))
        (apply fn args)))))

(define py2-runtime
#<<EOF
glo = {}
reg = [None for i in xrange(5)]
#stack = []
stack = {0:None}
sp = -1
nargs = 0

def lbl1_fx_3c_(): # fx<
    global glo
    global reg
    global stack
    global sp
    global nargs
    if nargs != 2:
        raise Exception("wrong number of arguments")
    reg[1] = reg[1] < reg[2]
    return reg[0]
glo["fx<"] = lbl1_fx_3c_

def lbl1_fx_2a_(): # fx*
    global glo
    global reg
    global stack
    global sp
    global nargs
    if nargs != 2:
        raise Exception("wrong number of arguments")
    reg[1] = reg[1] * reg[2]
    return reg[0]
glo["fx*"] = lbl1_fx_2a_


def lbl1_fx_2b_(): # fx+
    global glo
    global reg
    global stack
    global sp
    global nargs
    if nargs != 2:
        raise Exception("wrong number of arguments")
    reg[1] = reg[1] + reg[2]
    return reg[0]
glo["fx+"] = lbl1_fx_2b_

def lbl1_fx_2d_(): # fx-
    global glo
    global reg
    global stack
    global sp
    global nargs
    if nargs != 2:
        raise Exception("wrong number of arguments")
    reg[1] = reg[1] - reg[2]
    return reg[0]
glo["fx-"] = lbl1_fx_2d_

def lbl1_print():  # print
    global glo
    global reg
    global stack
    global sp
    global nargs
    if nargs != 1:
        raise Exception("wrong number of arguments")
    print reg[1]
    return reg[0]
glo["print"] = lbl1_print


def run(pc):
    while pc is not None:
        pc = pc()

EOF
)
