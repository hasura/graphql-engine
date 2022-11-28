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
			echo '# Then, run `make server/tests-py/requirements.txt` to regenerate this file.'; \
			echo; \
			pip3 freeze \
		) > $@

$(LEGACY_TESTS)/node_modules: $(LEGACY_TESTS)/package-lock.json
	@ cd $(LEGACY_TESTS); \
	npm_config_loglevel=error npm ci

$(LEGACY_TESTS)/package-lock.json: $(LEGACY_TESTS)/remote_schemas/nodejs/package.json
	@ cd $(LEGACY_TESTS); \
	npm_config_loglevel=error npm install --package-lock-only
