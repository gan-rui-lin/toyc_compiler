.text
.global main

f:
	addi sp, sp, -256
	sw a0, -4(sp)
	lw t1, -4(sp)
	li t2, 3
	rem t0, t1, t2
	sw t0, -8(sp)
	lw a0, -8(sp)
	addi sp, sp, 256
	ret

main:
	addi sp, sp, -256
	li t0, 0
	sw t0, -4(sp)
	li t0, 0
	sw t0, -8(sp)
	lw t1, -4(sp)
	li t2, 1
	sub t0, t1, t2
	sw t0, -12(sp)
	lw t0, -12(sp)
	sw t0, -16(sp)
	j L0
L0:
	lw t1, -4(sp)
	li t2, 5
	slt t0, t1, t2
	sw t0, -20(sp)
	lw t0, -20(sp)
	bne t0, x0, L1
	j L2
L1:
	lw t1, -4(sp)
	li t2, 1
	sub t0, t1, t2
	sw t0, -24(sp)
	lw t0, -24(sp)
	sw t0, -28(sp)
	lw t1, -28(sp)
	li t2, 1
	sub t0, t1, t2
	sw t0, -32(sp)
	lw a0, -32(sp)
	call f
	sw a0, -36(sp)
	lw t1, -36(sp)
	li t2, 0
	sub t0, t1, t2
	seqz t0, t0
	sw t0, -40(sp)
	lw t0, -40(sp)
	bne t0, x0, L3
	j L4
L3:
	j L0
L4:
	li t0, 0
	sw t0, -44(sp)
	j L5
L5:
	lw t1, -44(sp)
	li t2, 4
	slt t0, t1, t2
	sw t0, -48(sp)
	lw t0, -48(sp)
	bne t0, x0, L6
	j L7
L6:
	lw t1, -16(sp)
	lw t2, -44(sp)
	add t0, t1, t2
	sw t0, -52(sp)
	lw t0, -52(sp)
	sw t0, -56(sp)
	lw t1, -56(sp)
	li t2, 10
	sgt t0, t1, t2
	sw t0, -60(sp)
	lw t0, -60(sp)
	bne t0, x0, L8
	j L9
L8:
	j L7
L9:
	lw t1, -44(sp)
	li t2, 2
	rem t0, t1, t2
	sw t0, -64(sp)
	lw t1, -64(sp)
	li t2, 0
	sub t0, t1, t2
	seqz t0, t0
	sw t0, -68(sp)
	lw t0, -68(sp)
	bne t0, x0, L10
	j L11
L10:
	lw t1, -44(sp)
	li t2, 1
	add t0, t1, t2
	sw t0, -72(sp)
	lw t0, -72(sp)
	sw t0, -76(sp)
	j L5
L11:
	lw a0, -76(sp)
	call f
	sw a0, -80(sp)
	lw t1, -56(sp)
	lw t2, -80(sp)
	add t0, t1, t2
	sw t0, -84(sp)
	lw t0, -84(sp)
	sw t0, -88(sp)
	lw t1, -76(sp)
	li t2, 1
	add t0, t1, t2
	sw t0, -92(sp)
	lw t0, -92(sp)
	sw t0, -96(sp)
	j L5
L7:
	lw t1, -88(sp)
	li t2, 20
	sgt t0, t1, t2
	sw t0, -100(sp)
	lw t0, -100(sp)
	bne t0, x0, L12
	j L13
L12:
	j L2
L13:
	j L0
L2:
	lw a0, -88(sp)
	addi sp, sp, 256
	ret

