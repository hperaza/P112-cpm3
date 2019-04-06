	title	'LDRBIOS for CP/M 3'

;	CP/M-80 Version 3
;	LDRBIOS for P112
;	Version 1.0
;	Compile with M80

	.z80
Z80182	equ	1
	include	Z180.INC

;	maclib cpm3	; disk definition macros

	; BIOS Jump vector.

	jp boot		; initial entry on cold start
	jp wboot	; reentry on program exit, warm start

	jp const	; return console input status
	jp conin	; return console input character
	jp conout	; send console output character
	jp list		; send list output character
	jp auxout	; send auxiliary output character
	jp auxin	; return auxiliary input character

	jp home		; set disks to logical home
	jp seldsk	; select disk drive, return disk parameter info
	jp settrk	; set disk track
	jp setsec	; set disk sector
	jp setdma	; set disk I/O memory address
	jp read		; read physical block(s)
	jp write	; write physical block(s)

	jp listst	; return list device status
	jp sectrn	; translate logical to physical sector

	jp conost	; return console output status
	jp auxist	; return auxx input status
	jp auxost	; return aux output status
	jp devtbl	; return address of device def table
	jp ?cinit	; change baud  rate of device

	jp getdrv	; return address of disk drive table
	jp multio	; set multiple record count for disk I/O
	jp flush	; flush BIOS maintained disk caching

	jp ?move	; block move memory to memory
	jp ?time	; signal time and date operation
	jp bnksel	; select bank for code execution and default DMA
	jp setbnk	; select different bank for disk I/O DMA operation
	jp ?xmove	; set source and destination banks for one operation

	jp 0		; reserved for future expansion
	jp 0		; reserved for future expansion
	jp 0		; reserved for future expansion

boot:
wboot:
	ret

const:
	in0	a,(sccacnt)
	bit	2,a
	ld	a,0
	ret	z
	dec	a
	ret

conin:
	ret

conout:
	call	const
	jr	z,conout
	ld	a,c
	out0	(sccad),a
	ret

list:
	ret

auxout:
	ret

auxin:
	ret

home:
	ret

seldsk:
	ld	hl,dph
	ret

settrk:
	ld	(track),bc
	ret

setsec:
	ld	(sector),bc
	ret

setdma:
	ld	(dmaad),bc
	ret

read:
	mvi a,0 ! out p$fdcmd ! in p$fdstat ! ret

write:
	ret

listst:
	ret

sectrn:
	ld	l,c
	ld	h,b
	ex	de,hl
	add	hl,bc
	ld	l,(hl)
	ld	h,0
	ret

conost:
	ret

auxist:
	ret

auxost:
	ret

devtbl:
	ret

?cinit:
	ret

getdrv:
	ret

multio:
	ret

flush:
	ret

?move:
	ex	de,hl		; we are passed source in DE and dest in HL
	ldir			; use Z80 block move instruction
	ex	de,hl		; need next address in same regs
	ret

?time:
	ret

bnksel:
	ret

setbnk:
	ret

?xmove:
	ret

	; disk parameter header

dph 	dw	0		; translate table address
	db	0,0,0,0,0,0,0,0,0 ; BDOS scratch area
	db	0		; media flag
	dw	dpb		; disk parameter block
	dw	csv		; checksum vector
	dw	alv		; allocation vector
	dw	dirbcb		; DIRBCB
	dw	0FFFFh		; DTABCB
	dw	0FFFFh		; HASH
	db	0		; HASH bank

csv 	ds	16		; checksum vector
alv 	ds	31		; allocation vector

	; disk parameter block

dpbsd 	dpb	128,26,77,1024,64,2

	; sector translate table

trans	skew 26,6,1

	; directory buffer control block

dirbcb 	db	0FFh		; disk drive
	ds	3		; record position
	ds	1		; write buffer flag
	ds	1		; BDOS scratch byte
	ds	2		; track
	ds	2		; sector
	dw	dirb		; directory buffer

	; directory buffer

dirb 	ds	128

	end
