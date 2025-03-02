wine sbcl --eval "(ql:quickload :deploy)" \
		--load dsg2025.asd \
		--eval "(ql:quickload :dsg2025)" \
		--eval "(asdf:make :dsg2025)"
