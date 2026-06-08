# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository. These guidelines build on the common user's
guidelines at ~/.claude/CLAUDE.md locally or
<https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>
online.

User-facing documentation is in README.org.

This is an Elisp project, and should follow the user's Elisp guidelines.

mu4e is a runtime requirement; it ships with mu and is not an ELPA package, so it
is not listed in `Package-Requires`. The `Eask` file locates the installed mu4e
Lisp directory and adds it to `load-path`; override the location with the
`MU4E_LISP_DIR` environment variable.

## Build/Test Commands

- Run all checks, lints, autoformatters, and tests: `./check.sh`

## Code Style Guidelines

- Use `;;;###autoload` for functions meant to be autoloaded
