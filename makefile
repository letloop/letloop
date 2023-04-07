letloop: local/bin/letloop

local/bin/letloop: letloop.scm
	scheme --program letloop.scm compile /usr/lib/csv9.5.4/ta6le/ letloop.scm ./local/bin/letloop
	./local/bin/letloop
