letloop: local/bin/letloop

local/bin/letloop: letloop.scm letloop.md letloop.nfo
	scheme --program letloop.scm compile /usr/lib/csv9.5.4/ta6le/ letloop.scm letloop.exe
	mv letloop.exe ./local/bin/letloop
