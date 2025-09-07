; KBUG V 1.04
; Monitor program for the MC-10 back door board

; K. Willmott 2025
; tested, working

; minibug originally modified for NMIS-0021

; 2023-03-01 adapt for 6303Y
; 2023-05-18 first clean up, add some comments
; 2023-05-20 add feedback text, write "W", exec command "X"
; 2023-05-21 fixed stack initialization error
; 2023-05-22 add CPU vector jump table
; 2023-05-25 add primitive RAM test
; 2023-06-18 add external baud clock for 3MHz operation
; 2023-06-19 make alpha input case insensitive
; 2023-06-22 add clock stretching
; 2023=07-14 code formatting clean up
; 2023-10-20 add S record address relocation
; 2025-08-07 port to MC-10 back porch

; based on the original source
; COPYWRITE 1973, MOTOROLA INC
; REV 004 (USED WITH MIKBUG)

DEBUG	set	FALSE

; ACIA

baseACIA	equ	$B418
serialStat	equ	baseACIA
serialData	equ	baseACIA+1

; Address of boot or test code, RTS to return to monitor

XCALL	equ	$6000		;call test code here

; MC6803 internal RAM

INTRAM	equ	$80
EXTRAM	equ	$0100

NUMVEC		equ	7	; number of CPU vectors to create
NUMREG		equ	9	; number of CPU register bytes

; memory test variables

ramst	equ	EXTRAM	;external ram
ramend	equ	$8000		;memory limit

; Flash ROM is actually $C000-$FFFF
	org	$C000
	.fcc "Kbug 1.04 for MC-10 back porch, Ken Willmott 2025"

; start of boot ROM area
	org	$F800

; ENTER POWER ON Sequence

START:
; initialize ACIA
	LDA   #$03           ; Reset the ACIA
        STA   serialStat
        LDA   #$15           ; Configure the ACIA
        STA   serialStat

	IF DEBUG = FALSE

; set up vector jump table

	lds	#EXTRAM-1   ;SET STACK POINTER
	ldab	#NUMVEC
	ldaa	#$7E		;JMP INSTRUCTION
	ldx	#VECERR

NEXTVC:	pshx
	psha
	decb
	bne	NEXTVC

	ENDIF

;	lds	#STACK   ;SET STACK POINTER
	lds	#$4040

; run main program

	jmp	KBUG

; Utility routines follow
;

; INPUT ONE CHAR INTO A-REGISTER
GETCH:		lda	serialStat
		lsra
		bcc	GETCH
		lda	serialData
		cmpa	#$7F
		beq	GETCH     ;RUBOUT; IGNORE
		rts

; Make input case insensitive
; From p.718 Hitachi HD6301-3 Handbook

TPR:	cmpa	#'a'	;Entry point
	bcs	TPR1
	cmpa	#'z'
	bhi	TPR1
	anda	#$DF	;Convert lowercase to uppercase
TPR1:	rts

; Input a character with output echo
; implemented as an entry point to OUTCH

INCH:	bsr	GETCH
	bsr	TPR
	cmpa	#$0D
	beq	NOECHO

; OUTPUT ONE CHAR in Accumulator A
;

OUTCH:		pshb           ;SAVE B-REG
serPut:		ldb	serialStat
		lsrb
		lsrb
		bcc	serPut
		sta	serialData
		pulb
NOECHO:		rts

; Output a char string
; address of string in X

PRSTRN:	ldaa	,X  ;Get a char
	beq	PRDONE
	bsr	OUTCH
	inx
	bra	PRSTRN
PRDONE:	rts

; Report vector problem

VECERR:	ldx	#ERROUT
	jsr	PRSTRN
FREEZE:	bra	FREEZE     ;Suspend via endless loop

; boot time memory test

MEMTST:	ldx	#ramst
	stx	memtop

loop:	ldaa	0,x	; get byte
	coma		; invert bits
	tab		; save inverted copy in B

	staa	0,x	; save complement same place
	cmpb	0,x
	bne	done	; read not same as written

	coma		; invert again
	staa	0,x	; save original value same place

	inx		; look at next byte
	cpx	#ramend
	beq	done
	bra	loop

done:	stx	memtop
	rts
;
; end utility routines


; Monitor code begins
;

; INPUT HEX CHAR
;

INHEX:	bsr	INCH
	cmpa	#'0'
	bmi	C1       ;NOT HEX
	cmpa	#'9'
	ble	IN1HG    ;IS HEX
	cmpa	#'A'
	bmi	C1       ;NOT HEX
	cmpa	#'F'
	bgt	C1       ;NOT HEX
	suba	#'A'-'9'-1    ;MAKE VALUES CONTIGUOUS
IN1HG:	rts

; S-record loader
;

LOAD:	bsr	INCH
	cmpa	#'S'
	bne	LOAD    ;1ST CHAR NOT (S)
	bsr	INCH
	cmpa	#'9'
	beq	C1
	cmpa	#'1'
	bne	LOAD    ;2ND CHAR NOT (1)
	clr	CKSM     ;ZERO CHECKSUM
	bsr	BYTE     ;READ BYTE
	suba	#2
	staa	BYTECT   ;BYTE COUNT

; BUILD ADDRESS
	bsr	BADOFF

; STORE DATA
LOAD11:	bsr	BYTE
	dec	BYTECT
	beq	LOAD15   ;ZERO BYTE COUNT
	staa	,X        ;STORE DATA
	inx
	bra	LOAD11

LOAD15:	inc	CKSM
	beq	LOAD
LOAD19:	ldaa	#'?'      ;PRINT QUESTION MARK

	jsr	OUTCH
C1:	jmp	CONTRL

; BUILD ADDRESS
;

BADOFF:	bsr	BYTE     ;READ 2 FRAMES
		adda	srecof	; add high order address offset
		bra	BAD2

BADDR:	bsr	BYTE     ;READ 2 FRAMES
BAD2:		staa	XHI
		bsr	BYTE
		staa	XLOW
		ldx	XHI      ;(X) ADDRESS WE BUILT
		rts

; INPUT BYTE (TWO FRAMES)
;

BYTE:	bsr	INHEX    ;GET HEX CHAR
	asla
	asla
	asla
	asla
	tab
	bsr	INHEX
	anda	#$0F     ;MASK TO 4 BITS
	aba
	tab
	addb	CKSM
	stab	CKSM
	rts

; CHANGE MEMORY (M AAAA DD NN)
;

CHANGE:	bsr	BADDR    ;BUILD ADDRESS
	bsr	OUTS     ;PRINT SPACE
	bsr	OUT2HS
	bsr	BYTE
	dex
	staa	,X
	cmpa	,X
	bne	LOAD19   ;MEMORY DID NOT CHANGE
	bra	CONTRL

; WRITE MEMORY (M AAAA NN)
;

MWRITE:	bsr	BADDR    ;BUILD ADDRESS
	bsr	OUTS     ;PRINT SPACE
	bsr	BYTE
	staa	,X
	bra	CONTRL

;  formatted output entry points
;

OUTHL:	lsra	;OUT HEX LEFT BCD DIGIT
	lsra
	lsra
	lsra

OUTHR:	anda	#$F	;OUT HEX RIGHT BCD DIGIT
	adda	#$30
	cmpa	#$39
	bhi	ISALF
	jmp	OUTCH

ISALF:	adda	#$7
	jmp	OUTCH

OUT2H:	ldaa	0,X      ;OUTPUT 2 HEX CHAR
	bsr	OUTHL    ;OUT LEFT HEX CHAR
	ldaa	0,X
	bsr	OUTHR    ;OUT RIGHT HEX VHAR
	inx
	rts

OUT2HS:	bsr	OUT2H    ;OUTPUT 2 HEX CHAR + SPACE
OUTS:	ldaa	#$20     ;SPACE
	jmp	OUTCH    ;(bsr & rts)

; Monitor startup
;

KBUG:
	IF DEBUG = FALSE
	jsr	MEMTST	;check memory
	ENDIF

	ldx	#MOTD		;Print start up message
	jsr	PRSTRN

	ldx	#MMSG1	;Print memtest results
	jsr	PRSTRN
	ldx	#memtop
	jsr	OUT2H
	jsr	OUT2H
	ldx	#MMSG2
	jsr	PRSTRN

	ldx	#cmdhlp   ;Print commands message
	jsr	PRSTRN

	clra
	staa	srecof	;initialize S record offset

	bra	CONTRL


; PRINT CONTENTS OF STACK

PRINT:	ldx	#REGHDR   ;Print register titles
	jsr	PRSTRN
	tsx
	stx	SP       ;SAVE STACK POINTER
	ldab	#9
PRINT2:	bsr	OUT2HS   ;OUT 2 HEX & SPCACE
	DECB
	bne	PRINT2

CONTRL:	LDS	#STACK   ;SET STACK POINTER
	ldaa	#$D      ;CARRIAGE RETURN
	jsr	OUTCH
	ldaa	#$A      ;LINE FEED
	jsr	OUTCH
	ldx	#PROMPT   ;Print start up message
	jsr	PRSTRN

	jsr	INCH     ;READ CHARACTER
	tab
	jsr	OUTS     ;PRINT SPACE

	cmpb	#'X'		;Execute stored program
	bne	NOTQ
	jsr	XCALL
	jmp	KBUG

NOTQ:	cmpb	#'L'		;Load S-record
	bne	NOTL
	jmp	LOAD

NOTL:	cmpb	#'M'		;Modify
	bne	NOTM
	jmp	CHANGE

NOTM:	cmpb	#'W'		;Write
	bne	NOTW
	jmp	MWRITE

NOTW:	cmpb	#'P'		;Print
	beq	PRINT
	cmpb	#'G'		;Go
	bne	CONTRL
	rti			;Load registers and run

; Constant data section

MOTD:	.fcb $0D,$0A
	.fcc "*** Kbug 1.04 for MC-10 back door ***"
	.fcb $0D,$0A,0

cmdhlp:	.fcc "G(o),L(oad),P(roc),M(od),W(rite),X(ecute)?:"
       	.fcb $0D,$0A,0

PROMPT:	.fcc "KBUG->"
	.fcb 0

REGHDR:	.fcb $0D,$0A
	.fcc "CC B  A  XH XL PH PL SH SL"
	.fcb $0D,$0A,0

ERROUT:	.fcb $0D,$0A
	.fcc "Err - vector table entry no init"
	.fcb $0D,$0A,0

MMSG1:	.fcc "RAM test passed to $"
	.fcb 0

MMSG2:	.fcc "."
	.fcb $0D,$0A,0

; Processor hardware vectors
; There are seven, not including CPU Reset

	org	$FFF0

SIO:	.fdb	VSIO
TOI:	.fdb	VTOI
OCI:	.fdb	VOCI
ICI:	.fdb	VICI
IRQ1:	.fdb	VIRQ1
SWI:	.fdb	VSWI
NMI:	.fdb	VNMI
RES:	.fdb	START

; Data Section
; located in internal RAM
	IF DEBUG = FALSE

	org	INTRAM
	ELSE
	org	EXTRAM
	ENDIF

memtop:	.rmb	2
srecof:	.rmb	1
; END REGISTERS FOR GO command

CKSM:	.rmb	1        ;CHECKSUM
BYTECT:	.rmb	1        ;BYTE COUNT
XHI:	.rmb	1        ;XREG HIGH
XLOW:	.rmb	1        ;XREG LOW

	IF DEBUG = FALSE
	org	EXTRAM-(NUMVEC*3)-NUMREG-1	; below end of internal RAM
	ELSE
	org	EXTRAM+$100-(NUMVEC*3)-NUMREG-1	; above end of internal RAM

	ENDIF


STACK:	.rmb	1        ;STACK POINTER

; REGISTERS FOR GO command

	.rmb	1        ;CONDITION CODES
	.rmb	1        ;B ACCUMULATOR
	.rmb	1        ;A
	.rmb	1        ;X-HIGH
	.rmb	1        ;X-LOW
	.rmb	1        ;P-HIGH
	.rmb	1        ;P-LOW
SP:	.rmb	1        ;S-HIGH
	.rmb	1        ;S-LOW

; CPU vector jump table
; must be in RAM to be alterable

VSIO:    .rmb	3
VTOI:    .rmb	3
VOCI:    .rmb	3
VICI:    .rmb	3
VIRQ1:   .rmb	3
VSWI:    .rmb	3
VNMI:    .rmb	3

HERE	equ	*

	.END
