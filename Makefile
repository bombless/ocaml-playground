TARGET = build/s
SRC = tree.ml hash.ml make_string.ml

all: $(TARGET)
$(TARGET): $(SRC)
	ocamlopt -c -o build/tree.cmx tree.ml
	ocamlopt -c -o build/hash.cmx hash.ml
	ocamlopt -c -I build -o build/make_string.cmx make_string.ml
	ocamlopt -g -o build/s unix.cmxa build/tree.cmx build/hash.cmx build/make_string.cmx
clean:
	rm build/*
perf: $(TARGET)
	mkdir -p perf_logs
	perf record -o perf_logs/perf.data $(TARGET)
run: $(TARGET)
	$(TARGET)