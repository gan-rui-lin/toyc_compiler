.text
.global main

fib:
	addi sp, sp, -256
	mv t1, a0
	li t2, 1
	sub t0, t1, t2
	seqz t0, t0
	bne t0, x0, L0
	mv t1, a0
	li t2, 0
	sub t2, t1, t2
	seqz t2, t2
	mv t0, t2
	mv t1, t0
	j L1
L0:
	li t0, 1
	mv t1, t0
L1:
	mv t0, t1
	bne t0, x0, L2
	j L3
L2:
	li a0, 1
	addi sp, sp, 256
	ret
L3:
	mv t1, a0
	li t2, 1
	sub t3, t1, t2
	mv a0, t3
	call fib
	mv t4, a0
	mv t1, a0
	li t2, 2
	sub t6, t1, t2
	mv a0, t6
	call fib
	mv t5, a0
	mv t1, t4
	mv t2, t5
	add t3, t1, t2
	mv a0, t3
	addi sp, sp, 256
	ret

