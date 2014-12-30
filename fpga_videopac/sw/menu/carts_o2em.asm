e000	movp	a, @a
	ret
	db	"            ", 000h, 000h
e001	movp	a, @a
	ret
	db	"            ", 000h, 000h
e002	movp	a, @a
	ret
	db	"P. Lander   ", 001h, 000h
e003	movp	a, @a
	ret
	db	"Invade      ", 002h, 000h
e004	movp	a, @a
	ret
	db	"AMOK        ", 003h, 001h
e005	movp	a, @a
	ret
	db	"Bees        ", 005h, 003h
e006	movp	a, @a
	ret
	db	"Frogger     ", 009h, 003h
e007	movp	a, @a
	ret
	db	"K.C. Munch  ", 00dh, 001h
e008	movp	a, @a
	ret
	db	"K.C. Chase  ", 00fh, 001h
e009	movp	a, @a
	ret
	db	"Puzzle Piece", 011h, 003h
e00A	movp	a, @a
	ret
	db	"Q-Bert      ", 015h, 003h
e00B	movp	a, @a
	ret
	db	"Turtles     ", 019h, 003h
e00C	movp	a, @a
	ret
	db	"            ", 0aah, 055h
e00D	movp	a, @a
	ret
	db	"            ", 0aah, 055h

	;; number of entries
num_entries	equ	00dch
