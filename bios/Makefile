.PREFIX:
.PREFIX: .mac .rel

ZXCC = zxcc

SRCS = bioskrnl.mac \
	boot.mac \
	chario.mac \
	drvtbl.mac \
	fdio.mac \
	hdio.mac \
	ramdisk.mac \
	move.mac \
	misc.mac \
	time.mac \
	scb.mac

INCS = biosequ.inc \
	cpm3.inc \
	modebaud.inc \
	p112equ.inc \
	z180.inc

SUBS = bios.sub \
	bnkbios.sub

OBJS = $(SRCS:.mac=.rel)

#disk = /dev/fd0
disk = cpm3zpm3.img

banked: bnkbios3.spr

nonbanked: bios3.spr

$(OBJS): %.rel: %.mac
	$(ZXCC) m80 =$<

bios3.spr: $(OBJS)
	$(ZXCC) drlink -"bios3[os,q]=bioskrnl,boot,chario,drvtbl,fdio,hdio,ramdisk,move,misc,time,scb"
#	$(ZXCC) gencpm

bnkbios3.spr: $(OBJS)
	$(ZXCC) drlink -"bnkbios3[b,q]=bioskrnl,boot,chario,drvtbl,fdio,hdio,ramdisk,move,misc,time,scb"
#	$(ZXCC) gencpm

copy: bios3.spr bnkbios3.spr
	cpmrm -f p112 $(disk) 0:bios3.spr
	cpmrm -f p112 $(disk) 0:bnkbios3.spr
	cpmcp -f p112 $(disk) bios3.spr 0:
	cpmcp -f p112 $(disk) bnkbios3.spr 0:

copysrc:
	@for i in $(SRCS) $(INCS) $(SUBS); do \
	  echo Copying $$i... ; \
	  cpmrm -f p112 $(disk) 0:$$i ; \
	  cpmcp -f p112 $(disk) $$i 0: ; \
	done

loader:

clean:
	rm -f *.rel core *~ *.\$$\$$\$$

test:
	zxcc m80g =fdiotest
	zxcc m80g =flopasm
	zxcc drlink fdiotest
	zxcc drlink flopasm

cptest:
	cpmrm -f p112 $(disk) 0:fdiotest.com
	cpmrm -f p112 $(disk) 0:fdiotest.sym
	cpmrm -f p112 $(disk) 0:flopasm.com
	cpmrm -f p112 $(disk) 0:flopasm.sym
	cpmcp -f p112 $(disk) fdiotest.com 0:
	cpmcp -f p112 $(disk) fdiotest.sym 0:
	cpmcp -f p112 $(disk) flopasm.com 0:
	cpmcp -f p112 $(disk) flopasm.sym 0:
