SCHEME=scheme

LETLOOP='letloop'

letloop: local/bin/letloop ## Compile letloop into $(pwd)/local/bin/letloop

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	find lib/ -name "*.wpo" | xargs rm -rf
	find lib/ -name "*.so" | xargs rm -rf
	$(SCHEME) --libdirs lib --compile-imported-libraries --program letloop.scm compile lib letloop.scm
	mv a.out local/bin/letloop

check: letloop blake3 # Run checks
	LD_LIBRARY_PATH=$(PWD)/local/lib/ LETLOOP=$(LETLOOP) sh make-check.sh
	LD_LIBRARY_PATH=$(PWD)/local/lib/ $(LETLOOP) check lib/

local/lib/libblake3.so:
	rm -rf local/src/blake3
	mkdir -p local/src/blake3
	cd local/src &&	git clone --depth=1 https://github.com/BLAKE3-team/BLAKE3 blake3
	cd local/src/blake3/c/ && gcc -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c blake3_sse2_x86-64_unix.S blake3_sse41_x86-64_unix.S blake3_avx2_x86-64_unix.S blake3_avx512_x86-64_unix.S
	mkdir -p local/lib/
	cp local/src/blake3/c/libblake3.so local/lib/

blake3: local/lib/libblake3.so

todo: ## Things that should be done
	@grep -nR --color=always -B 2 -A 2 TODO lib/

xxx: ## Things that require attention
	@grep -nR --color=always -B 2 -A 2 XXX lib/
