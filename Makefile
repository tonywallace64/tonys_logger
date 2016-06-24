compile: ebin/tonys_logger.beam ebin/writer2.beam ebin/tonys_logger_app.beam ebin/writer_sup.beam ebin/tonys_logger.app ebin/tonys_logger_svr.beam

ebin/tonys_logger.app: src/tonys_logger.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	erlc -o ebin $< 

clean:
	-rm -f ebin/*
