SCHEME=scheme

LETLOOP='letloop'

letloop: local/bin/letloop ## Compile letloop into $(pwd)/local/bin/letloop
	@echo What is done is not to be done!

help: ## HELP!...
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	find lib/ -name "*.wpo" | xargs rm -rf
	find lib/ -name "*.so" | xargs rm -rf
	mkdir -p /tmp/letloop/
	$(SCHEME) --libdirs lib --compile-imported-libraries --program letloop.scm compile lib letloop.scm
	mv a.out local/bin/letloop

todo: ## So say we all...
	@grep -nR --color=always -B 2 -A 2 TODO lib/

xxx: ## We are all born under the eye of the wandering star
	@grep -nR --color=always -B 2 -A 2 XXX lib/

check: ## Hit the ground running...
	LD_LIBRARY_PATH=$(PWD)/local/lib/ LETLOOP=$(LETLOOP) sh make-check.sh
	LD_LIBRARY_PATH=$(PWD)/local/lib/ $(LETLOOP) check lib/
