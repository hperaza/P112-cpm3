.PREFIX:
.PREFIX: .mac .asm .rel

ZXCC = zxcc

loader: fcpmldr.com hcpmldr.com

ldrbios.rel: ldrbios.180
	$(ZXCC) m80 =$<

ldrhd.rel: ldrhd.asm
	$(ZXCC) m80 =$<

fcpmldr.com: ldrbios.rel
	$(ZXCC) drlink -"fcpmldr[l100]=cpmldr,$<"

hcpmldr.com: ldrhd.rel
	$(ZXCC) drlink -"hcpmldr[l100]=cpmldr,$<"

copy:
	cpmrm -f p112 /dev/fd0 0:fcpmldr.com
	cpmcp -f p112 /dev/fd0 fcpmldr.com 0:
	cpmrm -f p112 /dev/fd0 0:hcpmldr.com
	cpmcp -f p112 /dev/fd0 hcpmldr.com 0:

clean:
	rm -f ldrhd.rel ldrbios.rel hcpmldr.com fcpmldr.com
	rm -f core *~ *.\$$\$$\$$
