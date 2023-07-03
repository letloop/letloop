SCHEME=scheme

LETLOOP='$(SCHEME) --program letloop.scm'

letloop: local/bin/letloop

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	$(SCHEME) --program letloop.scm compile letloop.scm
	mv a.out letloop.exe

check:
	LETLOOP=$(LETLOOP) sh make-check.sh
