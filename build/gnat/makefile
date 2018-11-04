TOP			=	../..
SRC			=	$(TOP)/src
DOC_SRC		=	$(TOP)/doc

GEN			=	gen
DOC_GEN		=	$(GEN)/doc

DESTDIR		?=	/opt

GPRBUILD	=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean
INSTALL		=	install
CAT			=	cat

LISPY_MODE	?=	debug

#########################################################################################################################

all: lispy.gpr

.PHONY: lispy.gpr

# Add lib targets here.
lispy.gpr:
	$(GPRBUILD) -p -XLISPY_MODE=$(LISPY_MODE) -Plispy.gpr

# Add clean lib targets here

.PHONY: clean

clean:
	$(GPRCLEAN) -Plispy.gpr

#########################################################################################################################

.PHONY: todo

todo:
	grep -riw --color TODO $(SRC)
