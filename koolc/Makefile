all: .prerequisites_ok
	rm -f .prerequisites_ok
	rm -f kool-parser
	mkdir temp
	cd temp && \
	bnfc -m ../Kool.cf && \
	make && \
	mv TestKool ../kool-parser
	rm -rf temp
	@echo "---------------------------------------"
	@echo "Done!"
	@echo "kool-parser generated in current folder"

.prerequisites_ok:
	./check_needed_programs.sh