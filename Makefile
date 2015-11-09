SRC=src/

all: bnfc insc_jvm insc_llvm

bnfc: $(SRC)/Instant.cf
	cd $(SRC) && bnfc -m -haskell Instant.cf && make && cd ..

insc_jvm: $(SRC)RunJvmCompiler.hs $(SRC)/JvmCompiler.hs
	cd $(SRC) && ghc RunJvmCompiler.hs -o insc_jvm && cd ..

insc_llvm: $(SRC)RunLlvmCompiler.hs $(SRC)LlvmCompiler.hs
	cd $(SRC) && ghc RunLlvmCompiler.hs -o insc_llvm && cd ..

clean:
	cd $(SRC) && rm insc_llvm && rm insc_jvm && make distclean
