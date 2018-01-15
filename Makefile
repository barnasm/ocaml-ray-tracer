
OCB_FLAGS =  -tag thread -use-ocamlfind -quiet
OCB_FLAGS += -package camlimages.png -package graphics
OCB_FLAGS += -I src
# OCB_FLAGS += -I ~/.opam/system/lib/camlimages

# OCB_FLAGS +=	camlimages_core.cmxa graphics.cmxa \
# 		camlimages_graphics.cmxa camlimages_png.cmxa x

OCB = 		ocamlbuild $(OCB_FLAGS)

MAIN = main

all: 		native byte # profile debug

clean:
			$(OCB) -clean

# lib:
# 			$(OCB) libdemo.cma
# 			$(OCB) libdemo.cmxa
# 			$(OCB) libdemo.cmxs

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

# test: 		native
# 			echo '{"hello": "json"}' | ./main.native 

# .PHONY: 	all clean byte native profile debug lib sanity test
.PHONY: 	all clean byte native profile debug

