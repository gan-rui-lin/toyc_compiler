

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

