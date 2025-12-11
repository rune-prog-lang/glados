	.file	"float_c.c"
	.intel_syntax noprefix
# GNU C17 (GCC) version 13.2.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 13.2.0, GMP version 6.3.0, MPFR version 4.2.1, MPC version 1.3.1, isl version isl-0.20-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -g -O2 -fPIC -fstack-protector-strong -fno-strict-overflow -frandom-seed=dj4myy3kv9 --param=ssp-buffer-size=4
	.text
.Ltext0:
	.file 0 "/home/shmul95/Repositories/glados/RuneLang" "float_c.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%f"
	.text
	.p2align 4
	.globl	show_float
	.type	show_float, @function
show_float:
.LVL0:
.LFB23:
	.file 1 "float_c.c"
	.loc 1 3 30 view -0
	.cfi_startproc
	.loc 1 4 3 view .LVU1
.LBB6:
.LBI6:
	.file 2 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio2.h"
	.loc 2 84 1 view .LVU2
.LBB7:
	.loc 2 86 3 view .LVU3
.LBE7:
.LBE6:
# float_c.c:3: void show_float(float value) {
	.loc 1 3 30 is_stmt 0 view .LVU4
	sub	rsp, 8	#,
	.cfi_def_cfa_offset 16
.LBB10:
.LBB8:
# /nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio2.h:86:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	.loc 2 86 10 view .LVU5
	lea	rsi, .LC0[rip]	# tmp86,
	mov	edi, 2	#,
	mov	eax, 1	#,
.LBE8:
.LBE10:
# float_c.c:4:   printf("%f", value);
	.loc 1 4 3 view .LVU6
	cvtss2sd	xmm0, xmm0	# tmp85, tmp89
.LVL1:
.LBB11:
.LBB9:
# /nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio2.h:86:   return __printf_chk (__USE_FORTIFY_LEVEL - 1, __fmt, __va_arg_pack ());
	.loc 2 86 10 view .LVU7
	call	__printf_chk@PLT	#
.LVL2:
	.loc 2 86 10 view .LVU8
.LBE9:
.LBE11:
	.loc 1 5 3 is_stmt 1 view .LVU9
.LBB12:
.LBI12:
	.file 3 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio.h"
	.loc 3 82 1 view .LVU10
.LBB13:
	.loc 3 84 3 view .LVU11
# /nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio.h:84:   return putc (__c, stdout);
	.loc 3 84 10 is_stmt 0 view .LVU12
	mov	rax, QWORD PTR stdout@GOTPCREL[rip]	# tmp87,
	mov	edi, 10	#,
	mov	rsi, QWORD PTR [rax]	# stdout, stdout
.LBE13:
.LBE12:
# float_c.c:6: }
	.loc 1 6 1 view .LVU13
	add	rsp, 8	#,
	.cfi_def_cfa_offset 8
.LBB15:
.LBB14:
# /nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio.h:84:   return putc (__c, stdout);
	.loc 3 84 10 view .LVU14
	jmp	putc@PLT	#
.LVL3:
.LBE14:
.LBE15:
	.cfi_endproc
.LFE23:
	.size	show_float, .-show_float
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB24:
	.loc 1 8 16 is_stmt 1 view -0
	.cfi_startproc
	.loc 1 9 3 view .LVU16
.LVL4:
	.loc 1 11 3 view .LVU17
# float_c.c:8: int main(void) {
	.loc 1 8 16 is_stmt 0 view .LVU18
	sub	rsp, 8	#,
	.cfi_def_cfa_offset 16
# float_c.c:11:   show_float(23.0);
	.loc 1 11 3 view .LVU19
	movss	xmm0, DWORD PTR .LC1[rip]	#,
	call	show_float@PLT	#
.LVL5:
	.loc 1 12 3 is_stmt 1 view .LVU20
	movss	xmm0, DWORD PTR .LC2[rip]	#,
	call	show_float@PLT	#
.LVL6:
	.loc 1 13 3 view .LVU21
# float_c.c:14: }
	.loc 1 14 1 is_stmt 0 view .LVU22
	xor	eax, eax	#
	add	rsp, 8	#,
	.cfi_def_cfa_offset 8
	ret	
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.section	.rodata.cst4,"aM",@progbits,4
	.align 4
.LC1:
	.long	1102577664
	.align 4
.LC2:
	.long	1109917696
	.text
.Letext0:
	.file 4 "/nix/store/f2lglcw69yh2yyihxq6kvhpanc3s1n9p-gcc-13.2.0/lib/gcc/x86_64-unknown-linux-gnu/13.2.0/include/stddef.h"
	.file 5 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/types.h"
	.file 6 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/types/struct_FILE.h"
	.file 7 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/types/FILE.h"
	.file 8 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/stdio.h"
	.file 9 "/nix/store/jig62nn8174n4dlk05lqwsvs5wd2c64r-glibc-2.39-52-dev/include/bits/stdio2-decl.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.long	0x412
	.value	0x5
	.byte	0x1
	.byte	0x8
	.long	.Ldebug_abbrev0
	.uleb128 0xc
	.long	.LASF51
	.byte	0x1d
	.long	.LASF0
	.long	.LASF1
	.long	.LLRL4
	.quad	0
	.long	.Ldebug_line0
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.long	.LASF2
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.long	.LASF3
	.uleb128 0x5
	.long	.LASF11
	.byte	0x4
	.byte	0xd6
	.byte	0x17
	.long	0x44
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.long	.LASF4
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.long	.LASF5
	.uleb128 0xd
	.byte	0x8
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.long	.LASF6
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.long	.LASF7
	.uleb128 0x2
	.byte	0x1
	.byte	0x6
	.long	.LASF8
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.long	.LASF9
	.uleb128 0xe
	.byte	0x4
	.byte	0x5
	.string	"int"
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.long	.LASF10
	.uleb128 0x5
	.long	.LASF12
	.byte	0x5
	.byte	0x98
	.byte	0x19
	.long	0x77
	.uleb128 0x5
	.long	.LASF13
	.byte	0x5
	.byte	0x99
	.byte	0x1b
	.long	0x77
	.uleb128 0x3
	.long	0x9b
	.uleb128 0x2
	.byte	0x1
	.byte	0x6
	.long	.LASF14
	.uleb128 0xf
	.long	0x9b
	.uleb128 0x10
	.long	.LASF52
	.byte	0xd8
	.byte	0x6
	.byte	0x31
	.byte	0x8
	.long	0x211
	.uleb128 0x1
	.long	.LASF15
	.byte	0x33
	.byte	0x7
	.long	0x70
	.byte	0
	.uleb128 0x1
	.long	.LASF16
	.byte	0x36
	.byte	0x9
	.long	0x96
	.byte	0x8
	.uleb128 0x1
	.long	.LASF17
	.byte	0x37
	.byte	0x9
	.long	0x96
	.byte	0x10
	.uleb128 0x1
	.long	.LASF18
	.byte	0x38
	.byte	0x9
	.long	0x96
	.byte	0x18
	.uleb128 0x1
	.long	.LASF19
	.byte	0x39
	.byte	0x9
	.long	0x96
	.byte	0x20
	.uleb128 0x1
	.long	.LASF20
	.byte	0x3a
	.byte	0x9
	.long	0x96
	.byte	0x28
	.uleb128 0x1
	.long	.LASF21
	.byte	0x3b
	.byte	0x9
	.long	0x96
	.byte	0x30
	.uleb128 0x1
	.long	.LASF22
	.byte	0x3c
	.byte	0x9
	.long	0x96
	.byte	0x38
	.uleb128 0x1
	.long	.LASF23
	.byte	0x3d
	.byte	0x9
	.long	0x96
	.byte	0x40
	.uleb128 0x1
	.long	.LASF24
	.byte	0x40
	.byte	0x9
	.long	0x96
	.byte	0x48
	.uleb128 0x1
	.long	.LASF25
	.byte	0x41
	.byte	0x9
	.long	0x96
	.byte	0x50
	.uleb128 0x1
	.long	.LASF26
	.byte	0x42
	.byte	0x9
	.long	0x96
	.byte	0x58
	.uleb128 0x1
	.long	.LASF27
	.byte	0x44
	.byte	0x16
	.long	0x22a
	.byte	0x60
	.uleb128 0x1
	.long	.LASF28
	.byte	0x46
	.byte	0x14
	.long	0x22f
	.byte	0x68
	.uleb128 0x1
	.long	.LASF29
	.byte	0x48
	.byte	0x7
	.long	0x70
	.byte	0x70
	.uleb128 0x1
	.long	.LASF30
	.byte	0x49
	.byte	0x7
	.long	0x70
	.byte	0x74
	.uleb128 0x1
	.long	.LASF31
	.byte	0x4a
	.byte	0xb
	.long	0x7e
	.byte	0x78
	.uleb128 0x1
	.long	.LASF32
	.byte	0x4d
	.byte	0x12
	.long	0x5b
	.byte	0x80
	.uleb128 0x1
	.long	.LASF33
	.byte	0x4e
	.byte	0xf
	.long	0x62
	.byte	0x82
	.uleb128 0x1
	.long	.LASF34
	.byte	0x4f
	.byte	0x8
	.long	0x234
	.byte	0x83
	.uleb128 0x1
	.long	.LASF35
	.byte	0x51
	.byte	0xf
	.long	0x244
	.byte	0x88
	.uleb128 0x1
	.long	.LASF36
	.byte	0x59
	.byte	0xd
	.long	0x8a
	.byte	0x90
	.uleb128 0x1
	.long	.LASF37
	.byte	0x5b
	.byte	0x17
	.long	0x24e
	.byte	0x98
	.uleb128 0x1
	.long	.LASF38
	.byte	0x5c
	.byte	0x19
	.long	0x258
	.byte	0xa0
	.uleb128 0x1
	.long	.LASF39
	.byte	0x5d
	.byte	0x14
	.long	0x22f
	.byte	0xa8
	.uleb128 0x1
	.long	.LASF40
	.byte	0x5e
	.byte	0x9
	.long	0x52
	.byte	0xb0
	.uleb128 0x1
	.long	.LASF41
	.byte	0x5f
	.byte	0xa
	.long	0x38
	.byte	0xb8
	.uleb128 0x1
	.long	.LASF42
	.byte	0x60
	.byte	0x7
	.long	0x70
	.byte	0xc0
	.uleb128 0x1
	.long	.LASF43
	.byte	0x62
	.byte	0x8
	.long	0x25d
	.byte	0xc4
	.byte	0
	.uleb128 0x5
	.long	.LASF44
	.byte	0x7
	.byte	0x7
	.byte	0x19
	.long	0xa7
	.uleb128 0x11
	.long	.LASF53
	.byte	0x6
	.byte	0x2b
	.byte	0xe
	.uleb128 0x7
	.long	.LASF45
	.uleb128 0x3
	.long	0x225
	.uleb128 0x3
	.long	0xa7
	.uleb128 0x8
	.long	0x9b
	.long	0x244
	.uleb128 0x9
	.long	0x44
	.byte	0
	.byte	0
	.uleb128 0x3
	.long	0x21d
	.uleb128 0x7
	.long	.LASF46
	.uleb128 0x3
	.long	0x249
	.uleb128 0x7
	.long	.LASF47
	.uleb128 0x3
	.long	0x253
	.uleb128 0x8
	.long	0x9b
	.long	0x26d
	.uleb128 0x9
	.long	0x44
	.byte	0x13
	.byte	0
	.uleb128 0x3
	.long	0xa2
	.uleb128 0x12
	.long	0x26d
	.uleb128 0x3
	.long	0x211
	.uleb128 0x13
	.long	.LASF54
	.byte	0x8
	.byte	0x96
	.byte	0xe
	.long	0x277
	.uleb128 0x14
	.long	.LASF48
	.byte	0x8
	.value	0x264
	.byte	0xc
	.long	0x70
	.long	0x2a4
	.uleb128 0x6
	.long	0x70
	.uleb128 0x6
	.long	0x277
	.byte	0
	.uleb128 0x15
	.long	.LASF49
	.byte	0x9
	.byte	0x34
	.byte	0xc
	.long	0x70
	.long	0x2c0
	.uleb128 0x6
	.long	0x70
	.uleb128 0x6
	.long	0x26d
	.uleb128 0xa
	.byte	0
	.uleb128 0x16
	.long	.LASF55
	.byte	0x1
	.byte	0x8
	.byte	0x5
	.long	0x70
	.quad	.LFB24
	.quad	.LFE24-.LFB24
	.uleb128 0x1
	.byte	0x9c
	.long	0x328
	.uleb128 0x17
	.string	"a"
	.byte	0x1
	.byte	0x9
	.byte	0x9
	.long	0x2a
	.byte	0x4
	.long	0x42280000
	.uleb128 0x18
	.quad	.LVL5
	.long	0x328
	.long	0x30e
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x61
	.uleb128 0x7
	.byte	0xa4
	.uleb128 0x2a
	.byte	0x4
	.long	0x41b80000
	.byte	0
	.uleb128 0xb
	.quad	.LVL6
	.long	0x328
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x61
	.uleb128 0x7
	.byte	0xa4
	.uleb128 0x2a
	.byte	0x4
	.long	0x42280000
	.byte	0
	.byte	0
	.uleb128 0x19
	.long	.LASF56
	.byte	0x1
	.byte	0x3
	.byte	0x6
	.quad	.LFB23
	.quad	.LFE23-.LFB23
	.uleb128 0x1
	.byte	0x9c
	.long	0x3dc
	.uleb128 0x1a
	.long	.LASF57
	.byte	0x1
	.byte	0x3
	.byte	0x17
	.long	0x2a
	.long	.LLST0
	.long	.LVUS0
	.uleb128 0x1b
	.long	0x3dc
	.quad	.LBI6
	.byte	.LVU2
	.long	.LLRL1
	.byte	0x1
	.byte	0x4
	.byte	0x3
	.long	0x3ac
	.uleb128 0x1c
	.long	0x3ed
	.long	.LLST2
	.long	.LVUS2
	.uleb128 0xb
	.quad	.LVL2
	.long	0x2a4
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x55
	.uleb128 0x1
	.byte	0x32
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x54
	.uleb128 0x9
	.byte	0x3
	.quad	.LC0
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x61
	.uleb128 0x7
	.byte	0xa3
	.uleb128 0x3
	.byte	0xa5
	.uleb128 0x11
	.uleb128 0x2a
	.byte	0xa8
	.uleb128 0x31
	.byte	0
	.byte	0
	.uleb128 0x1d
	.long	0x3fb
	.quad	.LBI12
	.byte	.LVU10
	.long	.LLRL3
	.byte	0x1
	.byte	0x5
	.byte	0x3
	.uleb128 0x1e
	.long	0x408
	.byte	0xa
	.uleb128 0x1f
	.quad	.LVL3
	.long	0x288
	.uleb128 0x4
	.uleb128 0x1
	.byte	0x55
	.uleb128 0x1
	.byte	0x3a
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x20
	.long	.LASF58
	.byte	0x2
	.byte	0x54
	.byte	0x1
	.long	0x70
	.byte	0x3
	.long	0x3fb
	.uleb128 0x21
	.long	.LASF50
	.byte	0x2
	.byte	0x54
	.byte	0x20
	.long	0x272
	.uleb128 0xa
	.byte	0
	.uleb128 0x22
	.long	.LASF59
	.byte	0x3
	.byte	0x52
	.byte	0x1
	.long	0x70
	.byte	0x3
	.uleb128 0x23
	.string	"__c"
	.byte	0x3
	.byte	0x52
	.byte	0xe
	.long	0x70
	.byte	0
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0x21
	.sleb128 6
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0x21
	.sleb128 8
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x49
	.byte	0
	.uleb128 0x2
	.uleb128 0x18
	.uleb128 0x7e
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0x5
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0x13
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0x18
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0xb
	.uleb128 0x48
	.byte	0x1
	.uleb128 0x7d
	.uleb128 0x1
	.uleb128 0x7f
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x1f
	.uleb128 0x1b
	.uleb128 0x1f
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0xf
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x12
	.uleb128 0x37
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x13
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x14
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x15
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x16
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x7a
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x17
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1c
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0x18
	.uleb128 0x48
	.byte	0x1
	.uleb128 0x7d
	.uleb128 0x1
	.uleb128 0x7f
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x7a
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1a
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x1b
	.uleb128 0x1d
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x52
	.uleb128 0x1
	.uleb128 0x2138
	.uleb128 0xb
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x58
	.uleb128 0xb
	.uleb128 0x59
	.uleb128 0xb
	.uleb128 0x57
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1c
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x1d
	.uleb128 0x1d
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x52
	.uleb128 0x1
	.uleb128 0x2138
	.uleb128 0xb
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x58
	.uleb128 0xb
	.uleb128 0x59
	.uleb128 0xb
	.uleb128 0x57
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1e
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1f
	.uleb128 0x48
	.byte	0x1
	.uleb128 0x7d
	.uleb128 0x1
	.uleb128 0x82
	.uleb128 0x19
	.uleb128 0x7f
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x20
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x21
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x22
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x20
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x23
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_loclists,"",@progbits
	.long	.Ldebug_loc3-.Ldebug_loc2
.Ldebug_loc2:
	.value	0x5
	.byte	0x8
	.byte	0
	.long	0
.Ldebug_loc0:
.LVUS0:
	.uleb128 0
	.uleb128 .LVU7
	.uleb128 .LVU7
	.uleb128 0
.LLST0:
	.byte	0x6
	.quad	.LVL0
	.byte	0x4
	.uleb128 .LVL0-.LVL0
	.uleb128 .LVL1-.LVL0
	.uleb128 0x1
	.byte	0x61
	.byte	0x4
	.uleb128 .LVL1-.LVL0
	.uleb128 .LFE23-.LVL0
	.uleb128 0x6
	.byte	0xa3
	.uleb128 0x3
	.byte	0xa5
	.uleb128 0x11
	.uleb128 0x2a
	.byte	0x9f
	.byte	0
.LVUS2:
	.uleb128 .LVU2
	.uleb128 .LVU8
.LLST2:
	.byte	0x8
	.quad	.LVL0
	.uleb128 .LVL2-.LVL0
	.uleb128 0xa
	.byte	0x3
	.quad	.LC0
	.byte	0x9f
	.byte	0
.Ldebug_loc3:
	.section	.debug_aranges,"",@progbits
	.long	0x3c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0
	.value	0
	.value	0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	.LFB24
	.quad	.LFE24-.LFB24
	.quad	0
	.quad	0
	.section	.debug_rnglists,"",@progbits
.Ldebug_ranges0:
	.long	.Ldebug_ranges3-.Ldebug_ranges2
.Ldebug_ranges2:
	.value	0x5
	.byte	0x8
	.byte	0
	.long	0
.LLRL1:
	.byte	0x5
	.quad	.LBB6
	.byte	0x4
	.uleb128 .LBB6-.LBB6
	.uleb128 .LBE6-.LBB6
	.byte	0x4
	.uleb128 .LBB10-.LBB6
	.uleb128 .LBE10-.LBB6
	.byte	0x4
	.uleb128 .LBB11-.LBB6
	.uleb128 .LBE11-.LBB6
	.byte	0
.LLRL3:
	.byte	0x5
	.quad	.LBB12
	.byte	0x4
	.uleb128 .LBB12-.LBB12
	.uleb128 .LBE12-.LBB12
	.byte	0x4
	.uleb128 .LBB15-.LBB12
	.uleb128 .LBE15-.LBB12
	.byte	0
.LLRL4:
	.byte	0x7
	.quad	.Ltext0
	.uleb128 .Letext0-.Ltext0
	.byte	0x7
	.quad	.LFB24
	.uleb128 .LFE24-.LFB24
	.byte	0
.Ldebug_ranges3:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF23:
	.string	"_IO_buf_end"
.LASF31:
	.string	"_old_offset"
.LASF49:
	.string	"__printf_chk"
.LASF3:
	.string	"double"
.LASF26:
	.string	"_IO_save_end"
.LASF9:
	.string	"short int"
.LASF11:
	.string	"size_t"
.LASF36:
	.string	"_offset"
.LASF59:
	.string	"putchar"
.LASF20:
	.string	"_IO_write_ptr"
.LASF15:
	.string	"_flags"
.LASF22:
	.string	"_IO_buf_base"
.LASF27:
	.string	"_markers"
.LASF17:
	.string	"_IO_read_end"
.LASF40:
	.string	"_freeres_buf"
.LASF2:
	.string	"float"
.LASF35:
	.string	"_lock"
.LASF10:
	.string	"long int"
.LASF58:
	.string	"printf"
.LASF32:
	.string	"_cur_column"
.LASF52:
	.string	"_IO_FILE"
.LASF6:
	.string	"unsigned char"
.LASF48:
	.string	"putc"
.LASF8:
	.string	"signed char"
.LASF37:
	.string	"_codecvt"
.LASF5:
	.string	"unsigned int"
.LASF45:
	.string	"_IO_marker"
.LASF34:
	.string	"_shortbuf"
.LASF19:
	.string	"_IO_write_base"
.LASF43:
	.string	"_unused2"
.LASF16:
	.string	"_IO_read_ptr"
.LASF7:
	.string	"short unsigned int"
.LASF14:
	.string	"char"
.LASF55:
	.string	"main"
.LASF51:
	.string	"GNU C17 13.2.0 -masm=intel -mtune=generic -march=x86-64 -g -O2 -fPIC -fstack-protector-strong -fno-strict-overflow -frandom-seed=dj4myy3kv9 --param=ssp-buffer-size=4"
.LASF38:
	.string	"_wide_data"
.LASF39:
	.string	"_freeres_list"
.LASF41:
	.string	"__pad5"
.LASF46:
	.string	"_IO_codecvt"
.LASF50:
	.string	"__fmt"
.LASF4:
	.string	"long unsigned int"
.LASF21:
	.string	"_IO_write_end"
.LASF13:
	.string	"__off64_t"
.LASF12:
	.string	"__off_t"
.LASF28:
	.string	"_chain"
.LASF47:
	.string	"_IO_wide_data"
.LASF25:
	.string	"_IO_backup_base"
.LASF30:
	.string	"_flags2"
.LASF42:
	.string	"_mode"
.LASF18:
	.string	"_IO_read_base"
.LASF33:
	.string	"_vtable_offset"
.LASF24:
	.string	"_IO_save_base"
.LASF29:
	.string	"_fileno"
.LASF44:
	.string	"FILE"
.LASF57:
	.string	"value"
.LASF54:
	.string	"stdout"
.LASF53:
	.string	"_IO_lock_t"
.LASF56:
	.string	"show_float"
	.section	.debug_line_str,"MS",@progbits,1
.LASF1:
	.string	"/home/shmul95/Repositories/glados/RuneLang"
.LASF0:
	.string	"float_c.c"
	.ident	"GCC: (GNU) 13.2.0"
	.section	.note.GNU-stack,"",@progbits
