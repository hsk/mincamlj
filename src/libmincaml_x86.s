.section .text
.global min_caml_print_int
.global min_caml_print_int_tail
min_caml_print_int:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$0x10, %esp
min_caml_print_int_tail:
	pushl	%eax
	pushl	$format_int
	call	_printf
	addl	$0x10, %esp
	movl	%ebp, %esp
	popl	%ebp
	ret
.global min_caml_create_array
.global min_caml_create_array_tail
min_caml_create_array:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$0x10, %esp
min_caml_create_array_tail:
	movl	%eax, %ecx
	movl	%edi, %eax
create_array_loop:
	cmpl	$0, %ecx
	jne	create_array_cont
create_array_exit:
	addl	$0x10, %esp
	movl	%ebp, %esp
	popl	%ebp
	ret
create_array_cont:
	movl	%ebx, (%edi)
	decl	%ecx
	addl	$4, %edi
	jmp	create_array_loop
.section .rodata
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%lf"
.align 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x3ff00000
	.long	0x0
