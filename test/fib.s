.text
.global main

fib:
	addi sp, sp, -256
	sw a0, -4(sp)
	lw t1, -4(sp)
	li t2, 1
	sub t0, t1, t2
	seqz t0, t0
	sw t0, -8(sp)
	lw t0, -8(sp)
	bne t0, x0, L0
	lw t1, -4(sp)
	li t2, 0
	sub t0, t1, t2
	seqz t0, t0
	sw t0, -12(sp)
	lw t0, -12(sp)
	sw t0, -16(sp)
	j L1
L0:
	li t0, 1
	sw t0, -16(sp)
L1:
	lw t0, -16(sp)
	bne t0, x0, L2
	j L3
L2:
	li a0, 1
	addi sp, sp, 256
	ret
L3:
	lw t1, -4(sp)
	li t2, 1
	sub t0, t1, t2
	sw t0, -20(sp)
	lw a0, -20(sp)
	call fib
	sw a0, -24(sp)
	lw t1, -4(sp)
	li t2, 2
	sub t0, t1, t2
	sw t0, -28(sp)
	lw a0, -28(sp)
	call fib
	sw a0, -32(sp)
	lw t1, -24(sp)
	lw t2, -32(sp)
	add t0, t1, t2
	sw t0, -36(sp)
	lw a0, -36(sp)
	addi sp, sp, 256
	ret

