.text
 .global main
add:
	addi sp, sp, -256
	sw a0, 4(sp)
	sw a1, 8(sp)
	lw t1, 4(sp)
	lw t2, 8(sp)
	add t0, t1, t2
	sw t0, 12(sp)
	lw a0, 12(sp)
	addi sp, sp, 256
	ret

main:
	addi sp, sp, -256
	li a0, 116
	li a1, 105
	call add
	sw a0, 4(sp)
	lw t0, 4(sp)
	sw t0, 8(sp)
	lw a0, 8(sp)
	addi sp, sp, 256
	ret

