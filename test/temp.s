AST:

Function f(a) : int {
  Return(Binop(%, ID(a), Number(3)))
}

Function main() : int {
  Decl(x, Call(f, [Number(5)]))
  Return(ID(x))
}

IR:

function f(a):
LABEL0:
    t0 = a % 3
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
	lw t1, 4(sp)
	li t2, 3
	rem t0, t1, t2
	sw t0, 8(sp)
	lw a0, 8(sp)
	addi sp, sp, 256
	ret

main:
	addi sp, sp, -256
	li a0, 5
	call f
	sw a0, 4(sp)
	lw t0, 4(sp)
	sw t0, 8(sp)
	lw a0, 8(sp)
	addi sp, sp, 256
	ret

