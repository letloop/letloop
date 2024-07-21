SCHEME=scheme

LETLOOP='letloop'

letloop: sqlite3 blake3 argon2 termbox2 lsm1 cmark ## Compile letloop into $(pwd)/local/bin/letloop
	make local/bin/letloop
	@echo What is done is not to be done!

help: ## HELP!...
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	echo $(SCHEME)
	$(SCHEME) --version
	make clean
	mkdir -p /tmp/letloop/
	$(SCHEME) --libdirs library/ --compile-imported-libraries --program letloop.scm compile library letloop.scm
	mv a.out local/bin/letloop

local/lib/libblake3.so:
	rm -rf local/src/blake3
	mkdir -p local/src/blake3
	cd local/src &&	git clone --depth=1 https://github.com/BLAKE3-team/BLAKE3 blake3
	cd local/src/blake3/c/ && gcc -fPIC -DBLAKE3_NO_NEON=1 -DBLAKE3_USE_NEON=0 -DBLAKE3_NO_SSE2 -DBLAKE3_NO_SSE41 -DBLAKE3_NO_AVX2 -DBLAKE3_NO_AVX512 -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c
	mkdir -p local/lib/
	cp local/src/blake3/c/libblake3.so local/lib/

blake3: local/lib/libblake3.so

todo: ## So say we all!
	@grep -nR --color=always -B 2 -A 2 TODO library/

xxx: ## For those born under the eye of a wandering star...
	@grep -nR --color=always -B 2 -A 2 XXX library/

check: clean ## Hit the ground running!
	LD_LIBRARY_PATH=$(PWD)/local/lib/ LETLOOP=$(LETLOOP) sh make-check.sh
	LD_LIBRARY_PATH=$(PWD)/local/lib/ $(LETLOOP) check --fail-fast library/

local/lib/libargon2.so.1: argon2

argon2:
	rm -rf local/src/argon2
	mkdir -p local/src/
	git clone --depth=1 https://github.com/P-H-C/phc-winner-argon2 local/src/argon2
	cd local/src//argon2 && make -j$(shell nproc --ignore 1)
	mkdir -p local/lib
	cp local/src/argon2/libargon2.so.1 local/lib/

local/lib/termbox2.so:
	rm -rf local/src/termbox2
	mkdir -p local/src
	cd local/src/ && git clone https://github.com/termbox/termbox2
	cd local/src/termbox2/ && make -j$(shell nproc --ignore 1)
	mkdir -p local/lib
	cp --dereference local/src/termbox2/libtermbox2.so local/lib/

termbox2: local/lib/termbox2.so

seed: seed.scm clean local/bin/letloop
	letloop compile library/ seed.scm
	mv a.out local/bin/seed

lsm1: local/lib/lsm.so

local/lib/lsm.so:
	which tclsh || exit 42
	rm -rf local/src/sqlite
	mkdir -p local/src
	cd local/src && git clone --depth=1 https://github.com/sqlite/sqlite
	cd local/src/sqlite && cp ../../MakefileLSM .
	cd local/src/sqlite && make -j$(shell nproc --ignore 1) -f MakefileLSM lsm.so
	mkdir -p local/lib/
	cd local/src/sqlite && cp lsm.so ../../lib/


sqlite3: local/lib/sqlite3.so

local/lib/sqlite3.so:
	which tclsh || exit 42
	rm -rf local/src/sqlite
	mkdir -p local/src
	cd local/src && git clone --depth=1 https://github.com/sqlite/sqlite
	cd local/src/sqlite && ./configure --disable-tcl --prefix=$(PWD)/local/ && make -j$(shell nproc --ignore 1) && make install

cmark: local/lib/libcmark.so

local/lib/libcmark.so:
	rm -rf local/src/cmark
	mkdir -p local/src
	cd local/src && git clone --depth=1 https://github.com/amirouche/cmark
	cd local/src/cmark && make -j$(shell nproc --ignore 1) && make && cp build/src/libcmark.so* $(PWD)/local/lib/

www:
	$(LETLOOP) exec library/ make-www.scm

nix:  ## Compile letloop with nix
	nix-shell --packages pkgconfig zsh neovim gcc stdenv rlwrap curl cmake chez git lz4.dev libuuid.dev zlib.dev tcl pandoc foundationdb71.dev $(EXTRA) 

clean: ## Safe clean up
	find library/ -name "*.wpo" | xargs rm -rf
	find library/ -name "*.so" | xargs rm -rf
