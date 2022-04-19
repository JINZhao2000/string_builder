build:
	ocamlbuild src/string_builder.cmo src/string_builder.cmi
	
test: string_builder_test.byte
	./string_builder_test.byte

string_builder_test.byte: src
	ocamlbuild -use-ocamlfind -tag 'debug' -pkg ounit2 -I src test/string_builder_test.byte

clean:
	rm -rf _build
	rm -f string_builder_test.byte
	