# opam install camlimage

OCB_FLAGS =  -tag thread -use-ocamlfind -quiet
OCB_FLAGS += -package camlimages.png -package graphics
OCB_FLAGS += -I src

OCB = 		ocamlbuild $(OCB_FLAGS)

MAIN = main

all: 		native byte

clean:
			$(OCB) -clean

native:  	sanity
			$(OCB) $(MAIN).native

byte: 		sanity
			$(OCB) $(MAIN).byte

profile: 	sanity
			$(OCB) -tag profile $(MAIN).native

debug: 		sanity
			$(OCB) -tag debug $(MAIN).byte

sanity:
		ocamlfind query camlimages

.PHONY: 	all clean byte native profile debug

