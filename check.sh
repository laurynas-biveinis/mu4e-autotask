#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.
#
# Continue on errors but track them.
#
# Linters / tests / autoformatters:
#
# - Elisp:
#   - Syntax check via byte compilation (catches unbalanced parens etc.)
#   - elisp-autofmt (skipped on syntax errors to avoid runaway re-indentation)
#   - elisp-lint (skipped if anything failed so far)
#   - eask lint keywords / regexps
#   - ERT tests (skipped on syntax errors)
# - Org: org-lint on README.org
# - Shell: bash -n, shellcheck, shfmt
# - Markdown: mdl, prettier, textlint terminology
# - GitHub Actions / YAML: actionlint, zizmor, prettier

set -eu -o pipefail

readonly SHELL_FILES=(check.sh)

ERRORS=0
ELISP_SYNTAX_FAILED=0
SHELL_SYNTAX_FAILED=0

# Elisp

echo -n "Checking Elisp syntax... "
if eask recompile; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
	ELISP_SYNTAX_FAILED=1
fi

if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	if eask format elisp-autofmt; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

eask clean elc >/dev/null 2>&1 || true

if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if eask lint elisp-lint; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

eask clean elc >/dev/null 2>&1 || true

echo -n "Running eask lint keywords... "
if eask lint keywords; then
	echo "OK!"
else
	echo "eask lint keywords failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running eask lint regexps... "
if eask lint regexps; then
	echo "OK!"
else
	echo "eask lint regexps failed"
	ERRORS=$((ERRORS + 1))
fi

if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running all tests... "
	if eask run script test; then
		echo "OK!"
	else
		echo "ERT tests failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping ERT tests due to Elisp syntax errors"
fi

# Org

echo -n "Checking org files... README.org "
if eask run script org-lint; then
	echo "OK!"
else
	echo "org files check failed"
	ERRORS=$((ERRORS + 1))
fi

# Shell

echo -n "Checking shell syntax... ${SHELL_FILES[*]} "
if bash -n "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	SHELL_SYNTAX_FAILED=1
fi

if [ $SHELL_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${SHELL_FILES[*]} "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format all shell scripts... ${SHELL_FILES[*]} "
	if shfmt -w "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck and shfmt due to previous errors"
fi

# Markdown

echo -n "Checking Markdown files... $(echo ./*.md) "
if mdl --no-verbose ./*.md; then
	echo "OK!"
else
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown formatting... $(echo ./*.md) "
if prettier --log-level warn --check ./*.md; then
	echo "OK!"
else
	echo "prettier check for Markdown failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... $(echo ./*.md) "
if textlint --rule terminology ./*.md; then
	echo "OK!"
else
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
fi

# GitHub Actions / YAML

echo -n "Checking GitHub workflows... $(echo .github/workflows/*.yml) "
if actionlint .github/workflows/*.yml; then
	echo "OK!"
else
	echo "actionlint check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub Actions security... $(echo .github/workflows/*.yml) "
if zizmor .github/workflows/*.yml; then
	echo "OK!"
else
	echo "zizmor check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) "
if prettier --log-level warn --check .github/workflows/*.yml; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
