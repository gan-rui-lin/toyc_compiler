# AST:

# Function f(n) : int {
#   Return(Binop(%, ID(n), Number(3)))
# }

# Function main() : int {
#   Decl(x, Call(f, [Number(5)]))
#   Return(ID(x))
# }

# IR:

# function f(n):
#   t0 = n % 3
#   return t0
# function main():
#   t1 = call f(5)
#   x = t1
#   return x


# .text
#  .global main


main:
	addi sp, sp, -256
	sw ra, 4(sp)
	li a0, 5
	call f
	sw a0, 8(sp)
	lw t0, 8(sp)
	sw t0, 12(sp)
	lw a0, 12(sp)
	lw ra, 4(sp)
	addi sp, sp, 256
	ret

f:
	addi sp, sp, -256
	sw a0, 4(sp)
	sw ra, 8(sp)
	lw t1, 4(sp)
	li t2, 3
	rem t0, t1, t2
	sw t0, 12(sp)
	lw a0, 12(sp)
	lw ra, 8(sp)
	addi sp, sp, 256
	ret