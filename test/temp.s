AST:

Function f(n) : int {
  Return(Binop(%, ID(n), Number(3)))
}

Function main() : int {
  Decl(x, Call(f, [Number(5)]))
  Return(ID(x))
}

IR:

function f(n):
LABEL0:
    t0 = n % 3
    return t0
  terminator: return t0
    preds: []
    succs: []
function main():
LABEL2:
    t0 = call f(5)
    x = t0
    return x
  terminator: return x
    preds: []
    succs: []


.text
 .global main
f:
	addi sp, sp, -256
	sw a0, 4(sp)
	sw ra, 8(sp)
	li t1, 3
	rem t0, a0, t1
	mv a0, t0
	lw ra, 8(sp)
	addi sp, sp, 256
	ret

main:
	addi sp, sp, -256
	sw ra, 4(sp)
	li a0, 5
	call f
	mv t1, a0
	mv a0, t1
	lw ra, 4(sp)
	addi sp, sp, 256
	ret

