
	;; *******************************************************************
	;; Menu program for use with the multicart controller
	;;
	;; $Id: menu.asm,v 1.11 2007/06/07 17:43:11 arnim Exp $
	;;
	;; Compile this code with asl according to the settings made in
	;; Makefile.
	;; There are two versions of the include file for loading data to
	;; page 7:
	;; carts_o2em.asm
	;;   This is a simple subset of entries. The number of entries is
	;;   reduced so no use is made of the multicart controller itself.
	;;   Use this to emulate the application in o2em.
	;;   Note that you won't be able to switch to a cartridge thus this
	;;   menu program simply restarts after selecting a cartidge.
	;; carts.asm
	;;   Automatically generated cartridge/entry data by gen_carts.pl
	;;   This is intended to be the main flow to integrate cartridges in
	;;   the menu program.
	;;   For further details on generating/setting up the entry data
	;;   refer to gen_carts.pl
	;; *******************************************************************

	cpu	8048

	include	"g7000.h"


	;; define me if you want to highlight all lines that are used
	;; by timer IRQ execution
;TIMER_LINES	equ	1


	;;
	;; IRAM address definitions
	;;
last_joy	equ	020h
joy_del		equ	021h
stat		equ	022h	; status byte
linenum		equ	023h	; current line number
epos		equ	024h	; position of first displayed entry
stack		equ	025h	; 2 byte stack

	;;
	;; Timer constants
	;;
inittimval	equ	0eeh
linetimval	equ	0deh

	;; delay for automatic scrolling
	;; number of frames per step
joy_delay	equ	15

	;; number of lines per frame
num_lines	equ	005h

	;; X start position for lines
xstart	equ	020h

	;; X position of indicator sprite
spr_xpos	equ	014h
	;; Y position of indicator sprite
spr_ypos	equ	06ch

	;;
	;; multicard XRAM addresses
	;;
cart_2k_offset	equ	080h
cart_size_mask	equ	081h
cart_ebank	equ	082h
cart_mode	equ	0ffh


	org	0400h
	;;
	;; Jump table
	;;
	jmp	bootup		; RESET
	jmp	irq		; interrupt
	jmp	timer		; timer
	jmp	vsynchandler
	jmp	start		; after selectgame
	jmp	soundirq	; sound-interrupt


	;; *******************************************************************
	;; Boot code
	;;
bootup
	mov	r7, #0ffh
	sel	rb1
	call	init


	;; *******************************************************************
	;; Start of main code
	;;
start
	call	gfxoff

	call	extramenable

	;; setup entry position
	mov	r1, #epos
	clr	a
	mov	@r1, a

	call	dump_entries

	;; enable timer/counter interrupt
	en	tcnti

	;; signal positive status
	mov	r0, #stat
	mov	a, #080h
	mov	@r0, a

	call	vdcenable

	;; setup indicator sprite data
	mov	r0, #vdc_spr0_shape
	mov	r1, #000h		; data index
	mov	r2, #008h		; counter
spr_cpy_loop
	mov	a, r1
	call	sprite_data
	movx	@r0, a
	inc	r0
	inc	r1
	djnz	r2, spr_cpy_loop
	;; setup indicator sprite control
	mov	r0, #vdc_spr0_ctrl
	mov	a, #spr_ypos
	movx	@r0, a
	inc	r0
	mov	a, #spr_xpos
	movx	@r0, a
	inc	r0
	mov	a, #col_spr_red
	movx	@r0, a


	call	gfxon

	call	extramenable

	;; init last joystick state
	mov	r7, #000h
	;; init counter
	mov	r6, #joy_delay
jpoll_loop
	call	waitvsync
	mov	r1, #000h
	call	getjoystick
	;; check action button
	jf0	wait_no_button
	jmp	no_button
wait_no_button
	call	waitvsync
	call	getjoystick
	jf0	wait_no_button
	jmp	button_pressed

no_button
	;; check whether Y axis changed
	mov	a, r3
	cpl	a
	add	a, #001h
	add	a, r7
	jz	joy_nochange

	;; update markers
	mov	a, r3
	mov	r7, a
	mov	r6, #000h
	jmp	joy_eval

joy_nochange
	dec	r6
	mov	a, r6
	jz	joy_eval
	jmp	jpoll_loop

joy_eval
	mov	r6, #joy_delay
	;; check whether joystick is in neutral position
	mov	a, r3
	jz	jpoll_loop

	mov	r0, #epos
	;; up/down?
	jb7	joy_up

	;; check downwards
	mov	a, @r0
	mov	r1, a
	cpl	a
	add	a, #num_entries - num_lines + 1
	;; don't increment if already on last entry
	jz	jpoll_loop
	mov	a, r1
	inc	a
	jmp	joy_entry_move

joy_up
	;; check upwards
	mov	a, @r0
	;; don't decrement if already on entry 0
	jz	jpoll_loop
	dec	a

joy_entry_move
	mov	@r0, a

	;; save r7 and r6
	mov	r1, #last_joy
	mov	a, r7
	mov	@r1, a
	inc	r1
	mov	a, r6
	mov	@r1, a

	;; update entries
	call	dump_entries

	;; restore r6 and r7
	mov	r1, #joy_del
	mov	a, @r1
	mov	r6, a
	dec	r1
	mov	a, @r1
	mov	r7, a

	jmp	jpoll_loop

button_pressed
	mov	r0, #stat
	clr	a
	mov	@r0, a
	call	vdcenable
	stop	tcnt
	dis	tcnti
	dis	i
	call	gfxoff
	call	extramenable

	;; get entry parameters
	mov	r0, #epos
	mov	a, @r0
	add	a, #002h
	mov	r7, a

	call	select_entry
	add	a, #00eh	; index to entry offset
	mov	r7, a
	call	e000

	mov	r0, #cart_2k_offset

	;; set 2k offset
	movx	@r0, a
	inc	r0

	;; set size mask
	mov	a, r7
	inc	a
	call	e000
	movx	@r0, a

	;; select last valid entry bank
	;; this is coupled to the fact that ASL allows only
	;; up to 4k of assembled code:
	;; 0700h + 0800h = 0f00h
	mov	r0, #cart_ebank
	mov	a, #008h
	movx	@r0, a
	;; set application mode
	sel	rb0
	mov	r0, #cart_mode
	mov	a, #0a5h
	;; jump to end of 2k range
	;; -> wrap around after setting application mode
	jmp	07ffh
	
	;;
	;; *******************************************************************


	;; *******************************************************************
	;; Vertical sync handler
	;;
	;; Based on the status byte, it decides whether to start the
	;; timer interrupt chain for the five lines or not.
	;;
vsynchandler
	;; check for status
	mov	r0, #stat
	mov	a, @r0
	jb7	vsync_runtim
	jmp	vsyncirq

vsync_runtim
	stop	tcnt
	mov	r0, #linenum
	clr	a
	mov	@r0, a

	mov	a, #inittimval
	mov	t, a
	strt	cnt
	jmp	vsyncirq	; VSYNC-interrupt
	;;
	;; *******************************************************************


	;; *******************************************************************
	;; Timer IRQ handler
	;;
	;; If there are further lines to be displayed, it prints the next
	;; line and re-triggers the timer line interrupt.
	;;
timer
	sel	rb0
	stop	tcnt

	;; save A on stack
	mov	r0, #stack
	mov	@r0, a
	inc	r0
	;; save P1 on stack
	in	a, p1
	mov	@r0, a

	;; read line number to R7
	mov	r0, #linenum
	mov	a, @r0
	mov	r7, a
	;; check for last line
	cpl	a
	add	a, #num_lines
	cpl	a
	jz	tim_finished

	;; reload timer
	mov	a, #linetimval
	mov	t, a
	strt	cnt

	call	vdcenable

	IFDEF	TIMER_LINES
	mov	r0, #vdc_color
	mov	a, #col_bck_red
	movx	@r0, a
	ENDIF

	call	print_line

	IFDEF	TIMER_LINES
	mov	r0, #vdc_color
	mov	a, #col_bck_black
	movx	@r0, a
	ENDIF

tim_finished
	;; increment line number
	mov	r0, #linenum
	mov	a, r7
	inc	a
	mov	@r0, a

	;; pop P1 from stack
	mov	r0, #stack+1
	mov	a, @r0
	outl	p1, a

	;; pop A from stack
	dec	r0
	mov	a, @r0

	retr
	;;
	;; *******************************************************************


	org	0500h
	;;
	;; Look-up table for line -> Y position mapping
	;;
line2ypos
	add	a, #line2ypos_tab & 0ffh
	movp	a, @a
	ret
line2ypos_tab
	db	026h, 04ah, 06ch, 090h, 0b4h

	;;
	;; Look-up table for line -> XRAM address mapping
	;;
line2ramadr
	add	a, #line2ramadr_tab & 0ffh
	movp	a, @a
	ret
line2ramadr_tab
	db	007h, 01fh, 037h, 04fh, 067h


	;; *******************************************************************
	;; Subroutine print_line
	;;
	;; Copies an entry for a line from XRAM to VDC.
	;;
	;; Parameters:
	;; r7 - line number to be printed
	;;
	;; Modifies:
	;; r0, r1, r2, r3, r4
	;;
print_line
	;; get RAM address for this line
	mov	a, r7
	call	line2ramadr
	mov	r0, a

	mov	r1, #0a0h	; R1 = a0h VDC Control
	movx	a, @r1		; Turn off Grid, Foreground
	anl	a, #0d6h
	movx	@r1, a

	orl	p1, #07ch	; set : !kbscan !vdcen !ramen copyen
	anl	p1, #0e7h	; clear : !vdcen !ramen

	;; get Y pos for this line
	mov	a, r7
	call	line2ypos
	mov	r2, a		; r2 = Y pos
	mov	r3, #020h	; r3 = X pos
	mov	r4, #0ch	; r4 = loop counter

	mov	r1, #vdc_char0
print_loop
	;; r0 - RAM location pointer
	;; r1 - VDC location pointer
	;; r2 - Y position
	;; r3 - X position
	;; r4 - loop counter
	mov	a, r2		; save Y
	movx	@r1, a
	inc	r1
	mov	a, r3		; save X
	movx	@r1, a
	inc	r1
	movx	a, @r0		; copy character code
	movx	@r1, a
	inc	r0
	inc	r1
	movx	a, @r0		; copy character color
	movx	@r1, a
	inc	r0
	inc	r1
	mov	a, r3		; increment X pos by 008h
	add	a, #008h
	mov	r3, a
	djnz	r4, print_loop

	orl	p1, #0bch	; set : !kbscan !vdcen !ramen  lumen
	anl	p1, #0b7h	; clear : !vdcen copyen

	mov	r1, #0a0h	; turn on grid, foreground
	movx	a, @r1
	orl	a, #028h
	movx	@r1, a
	ret
	;;
	;; *******************************************************************


	;; *******************************************************************
	;; dump_entries
	;;
	;; Based on the topmost entry, it prepares the dump in XRAM of the
	;; currently displayed line.
	;; 
	;; Parameters:
	;; none
	;;
	;; Modifies:
	;; r0, r1, r2, r4, r5, r6, r7
	;;
dump_entries
	mov	r6, #col_chr_white
	;; line 0
	mov	r4, #000h
	mov	a, r4
	call	line2ramadr
	mov	r0, a
	mov	r1, #epos
	mov	a, @r1
	mov	r7, a
	call	dump_entry
	;; line 1
	mov	r4, #001h
	mov	a, r4
	call	line2ramadr
	mov	r0, a
	mov	r1, #epos
	mov	a, @r1
	mov	r7, a
	call	dump_entry
	;; line 2
	mov	r4, #002h
	mov	a, r4
	call	line2ramadr
	mov	r0, a
	mov	r1, #epos
	mov	a, @r1
	mov	r7, a
	mov	r6, #col_chr_red
	call	dump_entry
	mov	r6, #col_chr_white
	;; line 3
	mov	r4, #003h
	mov	a, r4
	call	line2ramadr
	mov	r0, a
	mov	r1, #epos
	mov	a, @r1
	mov	r7, a
	call	dump_entry
	;; line 4
	mov	r4, #004h
	mov	a, r4
	call	line2ramadr
	mov	r0, a
	mov	r1, #epos
	mov	a, @r1
	mov	r7, a
	call	dump_entry
	ret
	;;
	;; *******************************************************************


	;; *******************************************************************
	;; dump_entry
	;;
	;; Dumps one line from ROM to XRAM as specified by the parameters.
	;; The Y/char mapping is done here to save calculation performance
	;; during the timer IRQ service routine.
	;;
	;; Parameters:
	;; r0 - RAM location
	;; r4 - line number
	;; r6 - character color
	;; r7 - top entry number
	;;
	;; Modifies:
	;; r0, r1, r2, r4, r5, r7
	;;
dump_entry
	;; add line number to top entry
	mov	a, r4
	add	a, r7
	mov	r7, a

	;; store Y position to RAM
	mov	a, r4
	call	line2ypos
	mov	r4, a

	call	select_entry
	add	a, #002h	; index to entry text
	mov	r7, a

	;; byte counter
	mov	r2, #00ch
de_loop	mov	a, r7
	call	e000
	call	ascii2code
	mov	r5, a

	;; r0 - RAM location
	;; r2 - byte conter
	;; r4 - Y position
	;; r5 - character code
	;; r6 - character color
	;; r7 - index into entry
	call	calcchar23

	mov	a, r5
	movx	@r0, a		; save character
	inc	r0
	mov	a, r6
	movx	@r0, a		; save color
	inc	r0

	inc	r7		; increment entry index
	djnz	r2, de_loop
	
	ret
	;;
	;; *******************************************************************


	;; *******************************************************************
	;; select_entry
	;;
	;; Selects the given entry in r7.
	;; The upper 4 bits are used to set the entry bank while the
	;; lower 4 bits are converted to an index into the entry table and
	;; are returned in A.
	;;
	;; Parameters:
	;; r7 - entry number
	;;
	;; Modifies:
	;; r1
	;;
select_entry

	;; do banking based on upper 4 r7 bits
	mov	a, r7
	swap	a
	anl	a, #00fh
	mov	r1, #cart_ebank
	movx	@r1, a

	;; convert entry number in lower 4 bits into byte offset
	;; i.e. multiply by 16
	mov	a, r7
	swap	a
	anl	a, #0f0h

	ret
	;;
	;; *******************************************************************


	org	0600h

	;;
	;; ASCII -> character code translation table
	;;
ascii2code
	anl	a, #07fh
	movp	a, @a
	ret
	;; 002h
	db	                        02fh, 02fh, 02fh, 02fh
	db	02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh
	;; 010h
	db	02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh
	db	02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh, 02fh
	;; 020h
	db	00ch, 02fh, 02fh, 02fh, 00bh, 02fh, 02fh, 02fh
	db	02fh, 02fh, 029h, 010h, 02fh, 028h, 027h, 02eh
	;; 030h
	db	000h, 001h, 002h, 003h, 004h, 005h, 006h, 007h
	db	008h, 009h, 00ah, 02fh, 02fh, 02bh, 02fh, 00dh
	;; 040h
	db	02fh, 020h, 025h, 023h, 01ah, 012h, 01bh, 01ch
	db	01dh, 016h, 01eh, 01fh, 00eh, 026h, 02dh, 017h
	;; 050h
	db	00fh, 018h, 013h, 019h, 014h, 015h, 024h, 011h
	db	022h, 02ch, 021h, 02fh, 03bh, 02fh, 02fh, 02fh
	;; 060h
	db	02fh, 020h, 025h, 023h, 01ah, 012h, 01bh, 01ch
	db	01dh, 016h, 01eh, 01fh, 00eh, 026h, 02dh, 017h
	;; 070h
	db	00fh, 018h, 013h, 019h, 014h, 015h, 024h, 011h
	db	022h, 02ch, 021h, 02fh, 02fh, 02fh, 02fh, 02fh

	;;
	;; Indicator sprite data
	;;
sprite_data
	add	a, #sprite_data_tab & 0ffh
	movp	a, @a
	ret
sprite_data_tab
	db	000h, 030h, 060h, 0ffh, 060h, 030h, 000h, 000h


	IFDEF	FOR_O2EM
	org	0700h

	include	"carts_o2em.asm"

	ELSEIF

	org	0700h
e000
	include	"carts.asm"

	ENDIF


	;; Unfortunately, ASL only allows to assemble code in
	;; the range 0000h to 0fffh. Thus, not all entry banks
	;; can be used in this apporach.
	org	0fffh
	movx	@r0, a
