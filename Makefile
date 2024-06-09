.PHONY: all run clean

all:
	ocamlopt -O3 quantik.ml -o quantik.exe

run:
	ocamlopt -O3 quantik.ml -o quantik.exe 
	./quantik.exe

clean:
	rm quantik.exe quantik.cmi quantik.cmx quantik.o
