LEGACY_TESTS = server/tests-py
LEGACY_TESTS_VENV = $(LEGACY_TESTS)/.hasura-dev-python-venv

# cryptography v3.4.8 requires Rust dependencies by default, but we don't need them for our tests.
# The following environment variable disables this requirement.
# https://cryptography.io/en/3.4.8/faq.html#installing-cryptography-fails-with-error-can-not-find-rust-compiler
export CRYPTOGRAPHY_DONT_BUILD_RUST = 1

# Make's automatic variables such as `$<` and `$@` can be confusing.
# If you are unsure what they do, please consult the documentation.
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html

$(LEGACY_TESTS_VENV): $(LEGACY_TESTS)/requirements.txt
	rm -rf "$(LEGACY_TESTS_VENV)"
	python3 -m venv "$(LEGACY_TESTS_VENV)"
	source "$@/bin/activate"; \
		pip3 install -r $<
	touch $@

$(LEGACY_TESTS)/requirements.txt: $(LEGACY_TESTS)/requirements-top-level.txt
	rm -rf "$(LEGACY_TESTS_VENV)"
	python3 -m venv "$(LEGACY_TESTS_VENV)"
	source "$(LEGACY_TESTS_VENV)/bin/activate"; \
		pip3 install wheel; \
		pip3 install -r $<; \
		( \
			echo '# Do not modify this file directly. Instead, modify $<.'; \
			echo '# Then, run `make` in this directory to regenerate this file.'; \
			pip3 freeze \
		) > $@
