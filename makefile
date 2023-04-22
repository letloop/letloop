LETLOOP='scheme --program letloop.scm'

letloop: local/bin/letloop

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	scheme --program letloop.scm compile letloop.scm
	mv a.out letloop.exe

check:
	LETLOOP=$(LETLOOP) sh check.sh
