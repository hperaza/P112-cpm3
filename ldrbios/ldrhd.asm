	title	'LDRBIOS for CP/M 3'

;	CP/M-80 Version 3
;	LDRBIOS for P112 (Hard disk version)
;	Version 1.0
;	Compile with M80

	.z80
Z80182	equ	1
	include	Z180.INC

;	maclib cpm3	; disk definition macros

; Task File Register Definitions

IDEBase	equ	50h		; GIDE base address

IdeDOR	equ	IDEBase+6	; Digital Output Register
IdeDat	equ	IDEBase+8	; Data Register
IdeErr	equ	IDEBase+9	; Error Register
IdeSCnt	equ	IDEBase+0Ah	; Sector Count
IdeSNum	equ	IDEBase+0Bh	; Sector Number
IdeCLo	equ	IDEBase+0Ch	; Cylinder Low
IdeCHi	equ	IDEBase+0Dh	; Cylinder High
IdeSDH	equ	IDEBase+0Eh	; Drive and Head
IdeCmd	equ	IDEBase+0Fh	; Command / Status

; IDE Hard disk commands:

CmdHome	equ	10h		; Recalibrate
CmdRd	equ	20h		; Read Sector
CmdWr	equ	30h		; Write Sector
CmdInit	equ	91h		; Initialize Drive Params

; Macro: wait for ready (Non-Busy) signal active

WaitRdy	macro
	local	wait
wait:	in	a,(IdeCmd)
	rla
	jr	c,wait
	endm

; Macro: wait for DRQ signal

WaitDrq	macro
	local	wait
wait:	in	a,(IdeCmd)
	bit	3,a
	jr	z,wait
	endm


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
	di		; disable further interrupts
	call	hd$init
	ret

const:
	ret

conin:
	ret

conout:
	in0	a,(sccacnt)
	bit	2,a
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
	call	hd$read
	ret

write:
	ret

listst:
	ret

sectrn:
	ld	l,c
	ld	h,b
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


; Initialise the hard disk parameters. This routine assumes that the
; hard disk has been initialized by the P112 ROM (since we are booting
; from it). It stores the disk geometry and partition parameters passed
; from the secondary boot loader in the corresponding locations.

hd$init:
	ld	hl,cyls
	ld	a,(ix+0)	; cylinders
	ld	(hl),a
	inc	hl
	ld	a,(ix+1)
	ld	(hl),a
	inc	hl
	ld	a,(ix+2)	; heads
	ld	(hl),a
	inc	hl
	ld	a,(ix+3)	; sectors
	ld	(hl),a
	inc	hl
	ld	a,(ix+4)	; partition start
	ld	(hl),a
	inc	hl
	ld	a,(ix+5)
	ld	(hl),a
	ret

; Read data from Hard Disk

hd$read:
	call	hd$prep		; prepare task file registers
	WaitRdy
	ld	a,CmdRd
	out	(IdeCmd),a	; command: read sector data
	WaitRdy
	WaitDrq			; wait for DRQ to become active
	ld	hl,(dmaad)	; get target address
	ld	bc,IdeDat	; B = 0 (counter), C = I/O address
	inir
	inir			; read 512 data bytes (2 x 256)
	WaitRdy
	in	a,(IdeCmd)	; check final drive status
	and	10001001b	; Busy, DRQ, or Error?
	ret	z		; no: everything is ok
	ld	a,1		; return with A=1 on error
	ret

; Prepare hard disk for data transfer. The physical sector address
; is written to the appropriate IDE registers.

hd$prep:
	WaitRdy
	ld	hl,(pstart)	; track offset for this logical unit
	xor	a
	ld	de,(track)
	add	hl,de		; add track number
	adc	a,0		; with 20-bit result
	ld	b,4
mul16:	add	hl,hl		; multiply by 16
	adc	a,a		; with 20-bit result
	djnz	mul16
	ld	de,(sector)	; get sector number (0-based)
	add	hl,de		; add desired relative block number
	adc	a,0		; to 20-bit resulting block number
	ld	c,a

; This routine uses physical drive characteristics from included file.
; This routine computes Head, Sector and Track from a sequential block number
; defined by; Trk_Offset * 16 + Block #.  The Physical characteristics needed
; from the HDDEF.INC include file are: HDHDA = Number_of_Heads and
; HDSPT = Sectors_per_Track.  Computation of the CHS address is per:
;
;   Sector := (Block# MOD hdSPT)+1      (* Quotient1 := Block# DIV hdSPT *)
;   Head   := Quotient1 MOD hdHds       (* Quotient2 := Quotient1 DIV hdHds *)
;   Track  := Quotient2
;
; Prepare for Disk Read/Write by Preloading all Registers

	ld	a,(secs)	; load number of sectors per track
	ld	e,a
	call	divide		; divide CHL by E
	inc	a		; make sector number base at 1
	out	(IdeSNum),a	; send to GIDE register
	ld	a,(heads)	; get number of heads
	ld	e,a
	call	divide		; divide CHL (quotient from above) by E
	or	0A0h		; assume unit 0, master
	out	(IdeSDH),a	; send to GIDE register
	ld	a,l
	out	(IdeCLo),a
	ld	a,h
	out	(IdeCHi),a	; send cylinder number to GIDE
	ld	a,0AAh
	out	(IdeErr),a	; activate retries w/pattern in GIDE error reg
	ld	a,1		; one sector to read or write
	out	(IdeSCnt),a	; set sector count
	ret

; Divide 24-bit Number by 8-bit Number returning Quotient and Remainder
; Enter: CHL = 24-bit Unsigned Dividend
;	 E = 8-bit Unsigned Divisor
; Exit : CHL = 24-bit Quotient
;	 A = 8-bit Remainder
; Uses : AF,BC,HL

divide:	ld	b,24+1		; 25 times thru Loop
	xor	a		; Clear Remainder and Carry
Div:	adc	a,a		; Shift Accum Left + Carry
	sbc	a,e		;  Subtract Divisor
	jr	nc,Div0		; ..jump if it Worked
	add	a,e		; Else restore Accum & Carry
Div0:	ccf			; Flip Carry Bit
	adc	hl,hl		;  Shift any Carry into
	rl	c		;   Dividend/Quotient
	djnz	Div		;  ..loop til Done
	ret

;***********************************************************************

dmaad:	ds	2
track:	ds	2
sector:	ds	2

	; hard disk geometry (as received from boot loader)

cyls:	dw	1024		; cylinders
heads:	db	7		; heads
secs:	db	17		; sectors
pstart:	dw	1		; partition start (in virtual tracks)

	; disk parameter header

dph: 	dw	0		; translate table address
	db	0,0,0,0,0,0,0,0,0 ; BDOS scratch area
	db	0		; media flag
	dw	dpb		; disk parameter block
	dw	csv		; checksum vector
	dw	alv		; allocation vector
	dw	dirbcb		; DIRBCB
	dw	dtabcb		; DTABCB
	dw	0FFFFh		; HASH
	db	0		; HASH bank

csv: 	ds	(2048/4)+1	; checksum vector
alv:	ds	(3996/4)+2	; allocation vector

	; disk parameter block

dpb:	dw	64		; 128-byte sectors per logical track
	db	5		; block shift factor (4k blocks)
	db	31		; block mask
	db	1		; extent mask
	dw	3995		; max block number (disk size - 1)
	dw	2047		; directory entries - 1
	db	11111111b	; dir blocks alloc0
	db	11111111b	; dir blocks alloc1
	dw	(2048/4)+1	; check vector size
	dw	2		; reserved tracks
	db	2		; physical record shift factor (512-byte sectors)
	db	3		; physical record mask

	; directory buffer control block

dirbcb:	db	0FFh		; disk drive
	ds	3		; record position
	ds	1		; write buffer flag
	ds	1		; BDOS scratch byte
	ds	2		; track
	ds	2		; sector
	dw	dirb		; directory buffer

	; data buffer control block

dtabcb:	db	0FFh		; disk drive
	ds	3		; record position
	ds	1		; write buffer flag
	ds	1		; BDOS scratch byte
	ds	2		; track
	ds	2		; sector
	dw	dtab		; data buffer

	; directory buffer

dirb:	ds	512

	; data buffer

dtab:	ds	512

	end
