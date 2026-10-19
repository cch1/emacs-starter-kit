# Emacs Configuration Skills & Lessons Learned

A reference document capturing non-obvious behaviours, debugging
techniques, and design decisions discovered through empirical
investigation of this Emacs configuration.

---

## Repository hygiene

- `custom.el` is machine-written by Emacs's Custom UI and should stay
  separate from `init.el`. The pattern `(setq custom-file ...) (load-file
  custom-file)` in `init.el` is correct. Never hand-edit `custom.el` — Emacs
  warns against it and may clobber changes.
- `eca/` and `tree-sitter/` contain compiled platform-specific binaries and
  should be `.gitignore`d, like `elpa/`. They are reproducible on demand.
- `.dir-locals.el` is in the global `.gitignore` — personal editor settings
  are intentionally not committed to project repos.

---

## Environment: direnv and exec-path-from-shell

These two packages are complementary, not conflicting:

- **`exec-path-from-shell`** runs once at startup and copies selected
  variables (`PATH`, `MANPATH`, `ANTHROPIC_API_KEY`) from the login shell
  into Emacs. Only runs in GUI Emacs (guarded by `window-system` check).
  Provides the global baseline environment.
- **`direnv-mode`** watches buffer directories and layers per-project
  environment (from `.envrc`) on top of that baseline on every buffer switch.
  Works regardless of GUI vs terminal Emacs.

`PATH` is touched by both; direnv wins locally and that is intentional.

---

## `prettify-symbols-mode`: hook ordering and font-lock laziness

### Hook ordering problem

`prettify-symbols-mode` activates from `prog-mode-hook`, which fires
**before** `clojure-mode-hook`. This means it bakes its font-lock keywords
from `prettify-symbols-alist` before `lisp-editing-behavior` (running in
`clojure-mode-hook`) populates the full alist. Only symbols present in the
alist at activation time render — in practice, only `clojure-mode`'s built-in
`("fn" . ?ƒ)` entry.

**Fix:** toggle prettify at the end of `lisp-editing-behavior` to force a
re-read of the complete alist:

```elisp
(when prettify-symbols-mode
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))
```

### font-lock is lazy

`prettify-symbols-mode` uses `font-lock` (jit-lock) to apply character
compositions. Enabling the mode with `(prettify-symbols-mode +1)` only
*schedules* re-fontification — it does not apply compositions immediately.

**Consequence:** if you call `indent-region` immediately after enabling
prettify, font-lock has not yet applied compositions. `current-column` returns
raw character positions, not display positions, so the visual indentation is
not corrected.

**Fix:** call `(font-lock-ensure)` between enabling prettify and running
`indent-region` to force synchronous fontification:

```elisp
(prettify-symbols-mode +1)
(font-lock-ensure)         ; apply compositions NOW
(indent-region ...)        ; current-column now sees display widths
```

Disabling prettify does **not** have this problem: `prettify-symbols-mode -1`
calls `prettify-symbols--clean-up-compositions` synchronously, removing all
composition properties immediately.

### Verifying mode state via emacsclient

```elisp
;; Is prettify active?
(with-current-buffer "foo.clj" prettify-symbols-mode)

;; What is in the alist?
(with-current-buffer "foo.clj" prettify-symbols-alist)

;; Is a composition applied at point?
(get-text-property (point) 'composition)

;; What column does Emacs see? (counts display widths including compositions)
(current-column)
```

---

## Raw/visual indentation parity with prettify

### The problem

`prettify-symbols-mode` makes glyphs shorter than their raw text (e.g. `and`
→ `∧`, 3 chars → 1 char). `current-column` counts **display widths**
including composition widths. Therefore `indent-region` produces different
results depending on whether prettify is active:

- With prettify OFF: `b` aligns under raw `a` at column 7 (after `(and `)
- With prettify ON + compositions applied: `b` aligns under visual `a` at
  column 5 (after `(∧ `)

The saved file must use raw positions (for external tools like cljfmt). The
displayed buffer must use visual positions (for the user's eyes).

### The solution

Buffer-local save hooks, installed via `prettify-symbols-mode-hook` so they
apply to every buffer where prettify is active:

```elisp
(defun cch/before-save-unprettify ()
  (when prettify-symbols-mode
    (save-excursion
      (prettify-symbols-mode -1)   ; remove compositions synchronously
      (indent-region ...)          ; re-indent to raw positions
      (delete-trailing-whitespace))))

(defun cch/after-save-reprettify ()
  (save-excursion
    (prettify-symbols-mode +1)
    (font-lock-ensure)             ; apply compositions before indenting
    (indent-region ...)            ; re-indent to visual positions
    (set-buffer-modified-p nil)))  ; suppress modified flag

(defun cch/prettify-save-hooks ()
  (add-hook 'before-save-hook #'cch/before-save-unprettify nil t)
  (add-hook 'after-save-hook  #'cch/after-save-reprettify nil t))

(add-hook 'prettify-symbols-mode-hook #'cch/prettify-save-hooks)
```

This applies to **all** buffers with prettify active (Clojure, Emacs Lisp,
etc.), not just Clojure.

### Key empirical findings

- `clojure-align-forms-automatically` (column-aligning binding form values)
  does **not** conflict with cljfmt — cljfmt preserves intra-form alignment
  spaces. Tested directly with `clojure -T:cljfmt fix`.
- The `after-save-reprettify` re-indent only works correctly with
  `font-lock-ensure`; without it the re-indent is a silent no-op.

---

## Aligning clojure-mode indentation with cljfmt

### Global settings (cch-clojure.el)

| Setting | Value | Reason |
|---|---|---|
| `indent-tabs-mode` | `nil` | cljfmt uses spaces |
| `clojure-indent-style` | `'always-align` | matches cljfmt default |
| `clojure-align-forms-automatically` | default (`t`) | cljfmt preserves alignment — no conflict |
| `cider-dynamic-indentation` | `nil` | cljfmt ignores `:style/indent` metadata; CIDER using it would override `define-clojure-indent` entries and disagree with cljfmt |

### Per-project settings (.dir-locals.el)

Project-specific macro indentation belongs in `.dir-locals.el` as a
`define-clojure-indent` `eval` form, mirroring `.cljfmt.edn`'s
`:extra-indents`. This keeps editor indentation rules with the project, not
the global Emacs config.

**Mapping: `[[:inner 0]]` → `:defn`, `[[:block N]]` → `N`**

> **Warning: do NOT use integer `0` for `[[:inner 0]]`. It looks right but
> is wrong.** Integer `N` means "N special args, then body-style", but the
> body indentation only applies when the first body arg starts on a new
> line. In `clojure-indent-function`, position 0 is the operator itself, so
> with `0` the first body arg is at position 1. If it's on the first line,
> each later line is at position > 1 and goes to the "further non-special
> args" branch, which aligns under the arg above. Verified with a throwaway
> symbol and spec `0`:
>
> ```clojure
> (zz/log ::a        (zz/log
>         ::b          ::a
>         ::c)         ::b)
> ```
>
> `:defn` always indents 2 spaces with no position counting, which is what
> cljfmt's `[[:inner 0]]` produces.

```elisp
((clojure-mode
  (eval . (define-clojure-indent
            (u/log :defn) (u/trace :defn)   ; com.brunobonacci.mulog
            (h/html :defn)                   ; hiccup2.core
            (m/search :defn) ...             ; meander.epsilon
            (pxx 1)                          ; st.domain/pxx [[:block 1]]
            (assert! :defn)                  ; #re "assert!$"
            (synchronize :defn)))))          ; #re "synchronize$"
```

**Maintenance:** when `.cljfmt.edn` `:extra-indents` changes, update
`.dir-locals.el` to match.

### What clojure-mode cannot replicate (Layer 1 limits)

- **Regex rules** — cljfmt's `#re "assert!$"` matches any symbol ending that
  way. Emacs needs one explicit entry per name.
- **Alias resolution** — cljfmt reads `ns` forms dynamically. Emacs matches
  literal symbols only (`u/log` only works if the alias is literally `u`).
- **Whitespace inside forms** — cljfmt removes stray spaces and consecutive
  blank lines. Emacs doesn't.

Layer 2 (format on save via `clojure-lsp` which reads `.cljfmt.edn` directly)
would eliminate these gaps but conflicts with the prettify save hooks and adds
LSP round-trip latency. The pre-commit hook provides a sufficient safety net.

---

## Testing clojure-mode indentation against cljfmt

The gold standard for verifying that `clojure-mode` indentation matches
`cljfmt` is to run the real cljfmt against a test file and confirm it reports
no changes. This catches mapping errors (like `0` vs `:defn`) that are
invisible from Emacs alone.

### The test file

`~/.emacs.d/prettify_indent_test.clj` is a standalone Clojure file covering:

- **Cases 1–10:** `prettify-symbols-mode` correctness — forms using symbols
  that prettify to narrower glyphs (`fn`→`ƒ`, `and`→`∧`, `or`→`∨`,
  `not=`→`≠`, `>=`→`≥`, `<=`→`≤`, `partial`→`∂`, `comp`→`∘`). Goal:
  `indent-region` produces identical raw indentation with prettify ON or OFF.
- **Case 11:** `u/log` — a real-world form exercising a custom
  `define-clojure-indent` rule. Verifies that the `:defn` spec produces the
  same 2-space body indent that cljfmt generates from `[[:inner 0]]`.

The file is intentionally self-contained (no project dependencies) and lives
outside the st project so it can be opened without a REPL or LSP connection.

### The reference .cljfmt.edn (st project)

```edn
{:paths ["src" "resources" "test" "test-resources" "dev" "dev-resources"]
 :file-pattern #re "(\\.clj[crsx]?|\\.edn)$"
 :extra-indents {com.brunobonacci.mulog/log [[:inner 0]]
                 com.brunobonacci.mulog/trace [[:inner 0]]
                 hiccup2.core/html [[:inner 0]]
                 meander.epsilon/search [[:inner 0]]
                 meander.epsilon/find [[:inner 0]]
                 meander.epsilon/match [[:inner 0]]
                 meander.epsilon/rewrite [[:inner 0]]
                 st.domain/pxx [[:block 1]]
                 #re "assert!$" [[:inner 0]]
                 #re "synchronize$" [[:inner 0]]}
 :alias-map {u com.brunobonacci.mulog
             m meander.epsilon}
 :parallel? true
 :indent-line-comments? true}
```

The `:alias-map` is what lets cljfmt recognise `u/log` as
`com.brunobonacci.mulog/log` and apply the `[[:inner 0]]` rule. Emacs has no
equivalent — it matches literal symbols only — hence the `u/log` entry in
`.dir-locals.el`.

### How to run the test

1. **Load the indent rules into the running Emacs** (once per session, or
   after editing `.dir-locals.el`):

   ```bash
   emacsclient --eval '(define-clojure-indent
     (u/log :defn) (u/trace :defn) (h/html :defn)
     (m/search :defn) (m/find :defn) (m/match :defn) (m/rewrite :defn)
     (meander/search :defn) (meander/find :defn)
     (meander/match :defn) (meander/rewrite :defn)
     (pxx 1) (assert! :defn) (synchronize :defn))'
   ```

   In normal use these rules come from `.dir-locals.el` when you first open
   any file inside the st project. The explicit call above is only needed when
   testing the `~/.emacs.d/` test file in isolation.

2. **Re-indent and save via Emacs:**

   ```bash
   emacsclient --eval '(with-current-buffer
     (find-file-noselect "/Users/cch1/.emacs.d/prettify_indent_test.clj")
     (indent-region (point-min) (point-max))
     (save-buffer))'
   ```

3. **Run cljfmt from the st project** (so it reads `.cljfmt.edn`):

   ```bash
   cd ~/Development/SunTribe/st
   clojure -T:cljfmt check \
     :paths '["/Users/cch1/.emacs.d/prettify_indent_test.clj"]'
   ```

   Expected output: `All source files formatted correctly`

   Any diff reported by cljfmt reveals a discrepancy between Emacs and cljfmt
   indentation rules, which must be investigated and resolved.

### Lessons learned from initial testing

- **The root cause was the spec, not the environment.** The `u/log` failure
  came entirely from mapping `[[:inner 0]]` to `0` instead of `:defn` (see
  the warning above). A fresh `with-temp-buffer` in `clojure-mode` gave the
  same wrong result, which ruled out CIDER, buffer-local state and session
  history.

- **`define-clojure-indent` from `.dir-locals.el` probably depends on session
  order (expected from Emacs semantics, not observed).** The `eval` form only
  runs when Emacs visits a file inside the project, but it sets global symbol
  properties. So a file outside the project, like the test file, should only
  get the rules after some st file has been opened in the same session. The
  explicit `emacsclient --eval` call above avoids depending on this.

- **Stale buffer-local `clojure-get-indent-function` (seen, cause unknown,
  harmless here).** The test buffer had a buffer-local
  `clojure-get-indent-function` set to `cider--get-symbol-indent`, even though
  `cider-dynamic-indentation` was `nil`. CIDER only sets it when that option
  is non-nil, so the buffer probably enabled `cider-mode` before the option
  took effect. I didn't confirm that. It didn't cause the bug:
  `cider--get-symbol-indent` returned the same value as the static rule for
  `u/log`, and killing the variable changed nothing. It could still override
  static rules for symbols that have `:style/indent` metadata. Check with:

  ```bash
  emacsclient --eval '(with-current-buffer "foo.clj"
    (list clojure-get-indent-function
          (local-variable-p (quote clojure-get-indent-function))))'
  ```

  If it is buffer-locally set, kill it:

  ```bash
  emacsclient --eval '(with-current-buffer "foo.clj"
    (kill-local-variable (quote clojure-get-indent-function)))'
  ```

- **`clojure-indent-spec` vs `clojure-indent-function`.** `define-clojure-indent`
  sets both. `clojure--find-indent-spec` checks `clojure-get-indent-function`
  first (the CIDER hook point), then `clojure-indent-function`. `((:block 0))`
  and integer `0` are the modern and legacy forms of the same rule. The
  problem with `0` is what the rule means (see the warning above), not how
  it's stored.

- **One result is still unexplained.** Once, with the rules already set,
  `(with-temp-buffer (clojure-mode) (clojure--find-indent-spec "u/log"))`
  returned `nil`. Later the same lookup, given the string from
  `thing-at-point`, returned the expected value. I don't know why they
  differed. Don't treat a single `nil` from that call as proof that a rule is
  missing. Check the symbol property directly with
  `(get 'u/log 'clojure-indent-function)`.

---

## Debugging the live Emacs server with emacsclient

When the Emacs server is running, avoid guessing about its state. Query it
directly:

```bash
# Eval any expression in the running server
emacsclient --eval "(expression)"

# Check a function's live definition
emacsclient --eval "(symbol-function 'my-function)"

# Inspect buffer-local state
emacsclient --eval "(with-current-buffer \"foo.clj\" some-variable)"

# List open buffers by major mode
emacsclient --eval "(mapcar #'buffer-name
  (seq-filter (lambda (b) (with-current-buffer b (eq major-mode 'clojure-mode)))
              (buffer-list)))"

# Redefine a function without restarting
emacsclient --eval "(defun my-function () ...)"

# Apply a hook function to all matching buffers
emacsclient --eval "(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (eq major-mode 'clojure-mode)
      (my-hook-function))))"
```

**Note:** `emacsclient --eval` with multiple top-level forms only reliably
evaluates the first. Wrap multiple forms in `(progn ...)` or issue them as
separate calls.

---

## desktop-save-mode

`(desktop-save-mode 1)` saves and restores the Emacs session (open buffers,
cursor positions, window layout, major modes) between restarts. Companion
settings:

```elisp
(desktop-save-mode 1)
(setq desktop-auto-save-timeout 60)
```

---

## Miscellaneous

- `cch/prettify-extra` defines `("delta" . ?∂)` but is never added to any
  `prettify-symbols-alist` — dead code.
- `lisp/lsp.el` provides `cch-lsp` but is never `require`d — dead code.
- `cch-clojurescript.el` uses `figwheel-sidecar` (legacy, pre-figwheel-main).
- `cider` is `:pin melpa-stable` which may lag behind bug fixes.
- `cider` `:config` calls `(lisp-editing-behavior)` outside any buffer
  context, which sets the global (non-buffer-local) `prettify-symbols-alist`.
  This is a side effect; the buffer-local `setq` in `lisp-editing-behavior`
  when called as a hook takes precedence for Clojure buffers.
