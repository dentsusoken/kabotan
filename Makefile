ROS         = ros
LISP_IMPL   = sbcl

SYSTEM      = kabotan
TEST_SYSTEM = kabotan-test

.PHONY: run repl test test-pbt test-all clean

## execute without debugger
run:
	$(ROS) -L $(LISP_IMPL) run -- \
	  --disable-debugger \
	  --eval '(ql:quickload :$(SYSTEM))' \
	  --eval '(uiop:quit (kabotan:main))'

## execute with repl
repl:
	$(ROS) -L $(LISP_IMPL) run -- \
	  --eval '(ql:quickload :$(SYSTEM))'

## execute tests
## Set KABOTAN_TEST_VERBOSE=1 to see all error logs during testing
test:
	$(ROS) -L $(LISP_IMPL) run -- \
	--disable-debugger \
	--eval '(asdf:load-asd (merge-pathnames "kabotan-test.asd" *default-pathname-defaults*))' \
	--eval '(ql:quickload :$(TEST_SYSTEM))' \
  --eval '(uiop:quit (if (kabotan.tests:run-tests) 0 1))'

## execute property-based tests only
test-pbt:
	$(ROS) -L $(LISP_IMPL) run -- \
	--disable-debugger \
	--eval '(ql:quickload :$(TEST_SYSTEM))' \
	--eval '(fiveam:run! (quote kabotan.tests::property-behavior-preservation-suite))' \
	--eval '(fiveam:run! (quote kabotan.tests::property-test-coverage-suite))' \
	--eval '(fiveam:run! (quote kabotan.tests::property-export-preservation-suite))' \
	--quit

## execute all tests including property-based tests
test-all:
	@echo "Running unit tests..."
	@$(MAKE) test
	@echo ""
	@echo "Running property-based tests..."
	@$(MAKE) test-pbt

clean:
	rm -f *.fasl **/*.fasl *.x86f **/*.x86f

## E2E tests - UI functionality tests with mocks (fast)
test-e2e-ui:
	npx playwright test --project=ui-tests --reporter=list --quiet

## E2E tests - Integration tests with real LLM API (requires OPENAI_API_KEY)
test-e2e-integration:
	npx playwright test --project=integration-tests --reporter=list --quiet

## E2E tests - Run both UI and integration tests
test-e2e-all:
	npx playwright test --reporter=list --quiet
