# Changelog

<!-- Entries are one line per item and section headings repeat for every release,
     so the line-length and duplicate-heading rules don't fit this file. -->
<!-- markdownlint-disable MD013 MD024 -->

## master (unreleased)

### New features

- Alien indexing now honors Projectile's ignore rules. Previously `alien` (the default indexing method) delegated the whole walk to an external tool and applied none of `projectile-globally-ignored-files` / `-directories` / `-file-suffixes`, `projectile-global-ignore-file-patterns` or the `-` entries of a project's `.projectile`, which was a frequent source of confusion. The rules are now pushed down into the tool itself (`git ls-files` exclude pathspecs, `fd --exclude`), so the filtering still happens outside Emacs and `alien` stays fast; the few tools that can't express exclusions (svn, fossil, bzr, darcs, pijul, and the plain `find` fallback) have their output filtered in Emacs Lisp instead. Set the new `projectile-alien-honors-ignores` to nil to restore the old behavior. Dirconfig `+` keep and `!` unignore entries have no equivalent in the external tools and remain `hybrid`/`native` only. As a result `alien` projects will list fewer files than before - which is the point, but do check the new `projectile-alien-honors-ignores` if a file you expect goes missing.
- Add an optional Embark/Marginalia integration, wired via `with-eval-after-load` so neither package becomes a dependency. `embark-act` on a project file now targets the right file even when `default-directory` is a subdirectory, via a transformer that *augments* (never replaces) Embark's own `project-file` handling - it resolves a candidate against the Projectile root only when the file actually lives there, and otherwise defers to Embark, leaving non-Projectile completions untouched. Acting on a project candidate offers project actions (switch, vc, dired, remove) through the new `projectile-embark-project-map`; project prompts now use a `projectile-project` completion category, annotated by Marginalia's file annotator just like before.

### Changes

- Ignore matching is now case-sensitive under every indexing method. `native` and `hybrid` used to fold case when matching `projectile-globally-ignored-file-suffixes`, `projectile-global-ignore-file-patterns` and the dirconfig patterns - a side effect of `string-suffix-p`'s IGNORE-CASE argument and of `case-fold-search` defaulting to `t`, rather than a deliberate choice - so `.elc` also hid `BUILD.ELC`. The external tools behind `alien` can't express that, so the three methods disagreed; they now agree on the case-sensitive reading. Spell out both cases if you relied on the old behavior.
- Drop `.ensime_cache` and `.eunit` from the default `projectile-globally-ignored-directories`. ENSIME was archived in 2018 and `.eunit` is a rebar2 artifact; both just added noise to every project's ignore set. Add them back if you still need them.
- Fix the globs derived from `projectile-globally-ignored-directories` treating a leading `*` as a wildcard. It is a marker meaning "at any depth", so `*.osc` names the `.osc` directory and must not also match `foo.osc`. This affected the patterns handed to `project.el`'s `project-ignores` and to the ripgrep search path.
- Remove `projectile-warn-when-dirconfig-is-ignored` and the warning it controlled. It existed only to tell you that `alien` indexing was bypassing your `.projectile`, which it no longer does.
- Drop the standalone package headers (`Version`, `Package-Requires`) from `projectile-consult.el`. It's an optional module shipped inside the Projectile package, not a package of its own, and the phantom `Package-Requires` made build tooling treat it as one (e.g. it broke `eldev`-based test runs on Emacs 28.x by enforcing Consult's Emacs 29.1 floor on the whole project). Its runtime needs (Consult 2.0+, hence Emacs 29.1+) are unchanged and documented in the file and the manual.

## 3.2.1 (2026-07-13)

### Bugs fixed

- [#2094](https://github.com/bbatsov/projectile/issues/2094): Fix a `wrong-type-argument stringp` crash when running `projectile-search` (a 3.2.0 regression): the search-prompt tool tag now accepts the backend name symbol, not just a string.

## 3.2.0 (2026-07-12)

### New features

- Add reviewable read-only search commands, a search-only sibling of the reviewable replace UI.
  - `projectile-search-review` (`s R`) searches for a literal string; `projectile-search-regexp-review` (`s X`) searches for an Emacs regexp, honoring full Emacs regexp syntax.
  - Matches are gathered into a read-only `*projectile-search*` buffer, grouped by file, one `LINE:COL: CONTEXT` line per match with the matched span highlighted; there is no preview, no per-match toggle and no apply.
  - The buffer reuses the replace reviewer's navigation, case/regexp toggles (`c`/`x`), line and file filters (`k`/`d`/`K`/`D`), re-search (`g`) and grep-mode export (`e`).
  - `r` bridges the current search to the replace reviewer, carrying over the term, literal-ness and case setting and prompting only for the replacement.
  - A literal `projectile-search-review` accelerates its scan with ripgrep when `rg` is installed, streaming matches in near-instantly on large projects.
    - Controlled by `projectile-search-use-ripgrep` (default on); set it to nil to always use the pure-elisp scan.
    - The ripgrep fast-path follows ripgrep's ignore rules plus Projectile's ignore globs, which can differ slightly from the elisp path's file set (e.g. hidden files, symlinks); regexp search and the whole replace reviewer always use the portable elisp scan.
  - The commands are available from `projectile-dispatch` and the Projectile menu.
- [#1924](https://github.com/bbatsov/projectile/issues/1924): Add reviewable project-wide replace commands that let you preview matches and choose which to apply, instead of the blocking, file-by-file `query-replace` walk of `projectile-replace`.
  - `projectile-replace-review` (`R`) does a literal replace; `projectile-replace-regexp-review` does an Emacs-regexp replace whose replacement can reference capture groups.
  - Matches are gathered in Emacs Lisp, so the regexp command honors full Emacs regexp syntax and the preview reflects exactly what will be edited, including unsaved changes in open buffers.
  - The `*projectile-replace*` results buffer shows a per-file, per-match preview where each match can be toggled on or off; `!` (or `C-c C-c`) applies just the enabled ones, in any order.
  - Applying edits each file from the bottom up, edits open buffers in place under a single undo step, writes closed files back preserving their coding system, and skips buffers modified since the search rather than corrupting them.
  - The results buffer can reshape the search in place, each action re-scanning and re-rendering the previews:
    - `c` toggles case sensitivity (seeded from `case-fold-search`).
    - `x` toggles between literal and Emacs-regexp matching, refusing an invalid regexp rather than erroring.
    - `k`/`d` keep or flush the matches whose line matches a regexp; `K`/`D` do the same by file name; re-searching with `g` restores anything filtered away.
  - A status line at the top shows the term, replacement, match and file counts, the mode flags, and a note when the list has been filtered.
  - `e` exports the shown matches to a `grep-mode` buffer so wgrep or Emacs 31's `grep-edit-mode` can turn them editable and write back; wgrep stays an optional integration and `!` remains the no-dependency apply path.
  - Both reviewers scan the project asynchronously, so a large search no longer freezes Emacs.
    - The results buffer opens right away, matches stream in as they're found (the header shows a "Searching..." progress note), and the scan is cancelable with `q`, `C-g`, or by killing the buffer.
    - While a scan is still running, `!` (apply), `e` (export) and the filter keys (`k`/`d`/`K`/`D`) refuse until it finishes, so the write-back never runs against a partial match set and a filter can't be outrun by a later chunk; starting a new scan (`g`/`c`/`x`) cancels any in-flight one.
    - Set `projectile-replace-async` to nil to force the old synchronous single-pass scan; batch runs always scan synchronously.
  - The commands are available from `projectile-dispatch` and the Projectile menu, and the match cap is customizable via `projectile-replace-max-matches`.
- Add `projectile-session-mode`, a global minor mode that gives each project its own `tab-bar` tab.
  - Switching to a project selects its existing tab (restoring that project's window layout) when one is open, or otherwise opens a fresh tab named after the project and populated via `projectile-session-default-action`.
  - Same-named checkouts get distinct tab names (e.g. `work/foo` and `home/foo`); customize the scheme with `projectile-session-tab-name-function`.
  - `projectile-session-switch-to-buffer` completes over just the current tab's project buffers.
  - Sessions persist across Emacs restarts: `projectile-session-save`, `projectile-session-restore` and `projectile-session-forget` write a project's window layout and buffers to a file under `projectile-session-directory` and read it back; `projectile-session-save-all` saves every open project at once.
  - Switching to a project with a saved session restores it (gated by `projectile-session-restore-on-switch`); `projectile-session-autosave` saves sessions on switch-away and on exit.
  - `projectile-session-restore-all` reopens every saved project's session, each into its own tab (skipping projects already open and dropping stale sessions whose files are gone); set `projectile-session-restore-on-startup` to run it automatically at Emacs startup.
  - Which buffers are recorded, and how, is extensible via `projectile-session-buffer-serializers`; file-visiting and `dired-mode` buffers work out of the box.
  - The session commands are bound under the `w` sub-prefix of the command map (`s-p w s`, `s-p w S`, `s-p w r`, `s-p w R`, `s-p w f`, `s-p w b`), and are also available from `projectile-dispatch` and the Projectile menu.

### Changes

- Add `projectile-frecency-max-projects` (default 100), which caps how many projects' frecency history is kept so the store can't grow without bound.
- `projectile-replace-scan-chunk-size` is now a user-facing `defcustom` (was an internal variable).
- Advertise the right completion metadata category per command so marginalia and embark annotate and act on candidates correctly: buffer switching now uses the `buffer` category (so marginalia shows its rich buffer annotations), project switching uses `file`, and file commands keep `project-file` - previously every completion was hardcoded to `project-file`. `projectile-completing-read` gained a `:category` keyword for this.
- Add whole-word matching to the reviewable search/replace: toggle it with `w` in the results buffer, default it with `projectile-search-whole-word`, or seed it for one run with the `projectile-dispatch` `--word` switch. The elisp scan fences the pattern with word boundaries and the ripgrep fast-path passes `--word-regexp`, so both engines agree.
- Wire the reviewable search/replace into the `projectile-dispatch` Modifiers: the two literal/regexp search entries are folded into one `sR` driven by `--regexp` (matching how `--regexp` already drives the ag/ripgrep search), and a new `--case-sensitive` switch seeds the reviewable search/replace case-sensitive. Both can still be flipped with `x`/`c` in the results buffer.
- Highlight the search tool and the default value in the search prompts, and give them one consistent `Search [tool] for (default: X)` format across `projectile-search`, `projectile-grep`/`-ag`/`-ripgrep` and the reviewable search. The tool that will run (`grep`/`ag`/`ripgrep`, or `ripgrep`/`elisp` for the reviewable search's literal fast-path) is faced with `projectile-search-prompt-tool` and the symbol-at-point default with `projectile-search-prompt-default`. The reviewable search now also shows its default up front and which engine it will use.

### Bugs fixed

- [#1549](https://github.com/bbatsov/projectile/issues/1549), [#1676](https://github.com/bbatsov/projectile/issues/1676): Re-invoke a function-valued lifecycle command (e.g. a CMake preset picker or a project type's `:run`/`:test` function) on every run instead of freezing its first result in the command cache, so it can prompt again.

## 3.1.0 (2026-07-04)

### New features

- Add declarative "file kinds", letting a project type describe categories of files (e.g. Rails models, controllers and views) via the new `:file-kinds` keyword of `projectile-register-project-type`.
  - `projectile-find-file-of-kind` (`j`) prompts for a kind and completes over just the project files of that kind.
  - `projectile-toggle-related-file` (`J`) generalizes `projectile-toggle-between-implementation-and-test`: it jumps to related files of other kinds, jumping straight when there's one, prompting when there are several, and cycling through them on repeated presses.
  - The `rails-test`, `rails-rspec` and `django` project types ship with ready-made `:file-kinds` tables. Related files are keyed by their namespaced path, so a top-level `UsersController` relates to the top-level `User` model rather than an `Admin::User`.
- `projectile-find-file` and `projectile-find-file-dwim` now rank the files you work with first, ordering completion candidates by how often and how recently you've visited them (with decay).
  - The ranking is applied through completion metadata, so it works with any completion UI and under every indexing method, including `alien` (which `projectile-sort-order` never reached).
  - Controlled by `projectile-enable-frecency` (default on); the per-project history is persisted in `projectile-frecency-file` and capped by `projectile-frecency-max-files`.
- [#1992](https://github.com/bbatsov/projectile/issues/1992), [#1587](https://github.com/bbatsov/projectile/issues/1587), [#1553](https://github.com/bbatsov/projectile/issues/1553), [#1794](https://github.com/bbatsov/projectile/issues/1794): Add named project tasks - shell commands (or functions returning them) that you can run by name.
  - `projectile-tasks` maps task names to commands and can be set globally, per project type via the new `:tasks` keyword of `projectile-register-project-type`, or per project via `.dir-locals.el`.
  - `projectile-run-task` (`c x`) picks a task with completion and runs it like the lifecycle commands, in a per-task compilation buffer; a prefix argument lets you edit the command first (e.g. to pass ad-hoc arguments).
  - `projectile-repeat-last-task` (`c X`) re-runs the project's last task.
- [#978](https://github.com/bbatsov/projectile/issues/978): Add `projectile-project-changed-functions`, run whenever the current project changes - including implicitly via visiting a file or directory of another project - with the new and previous project root as arguments.
- [#1442](https://github.com/bbatsov/projectile/pull/1442): `projectile-sort-order` can now be set to a function that receives the list of project files and returns them in the desired order.
- [#1984](https://github.com/bbatsov/projectile/pull/1984): The VCS markers are now customizable via `projectile-vcs-markers`, whose order breaks ties between markers in the same directory - so colocated `jj`+`git` repositories can be detected as `jj` by moving `.jj` first.
- [#1890](https://github.com/bbatsov/projectile/pull/1890): Recognize osc (openSUSE Build Service) checkouts: `.osc` is now a VCS marker, a top-down-recurring root marker, and globally ignored; file listing uses the generic indexing command.
- [#1694](https://github.com/bbatsov/projectile/issues/1694): Add `projectile-invalidate-cache-all`, which invalidates the caches of all known projects at once (handy when commands like `projectile-find-file-in-known-projects` serve stale results).
- [#1075](https://github.com/bbatsov/projectile/issues/1075): Add experimental opt-in automatic cache updates via filesystem notifications, so files created, deleted or renamed outside Emacs update the cache without a manual `projectile-invalidate-cache`.
  - Enable it with `projectile-auto-update-cache-with-watches`; only local, cached projects are watched.
  - Each project uses one `file-notify` watch per directory, bounded by `projectile-watch-directory-limit`.
- Add `projectile-other-window-command` (`s-p 4 4`) and `projectile-other-frame-command` (`s-p 5 5`), modeled after Emacs's `other-window-prefix`/`other-frame-prefix` (`C-x 4 4`/`C-x 5 5`): they display the buffer of the next command in another window or frame, keeping the Projectile keymap active for the next key.
  - This works with any command, including ones without a dedicated `-other-window`/`-other-frame` variant, e.g. `s-p 4 4 x s` starts a project shell in another window.
- Add `projectile-run-test-at-point` (bound to `c .`), which runs just the test around point, located via the buffer's tree-sitter parse tree (requires Emacs 29+ with tree-sitter).
  - Rules for pytest (`python-ts-mode`), `go test` (`go-ts-mode`) and jest (`js-ts-mode`/`typescript-ts-mode`/`tsx-ts-mode`) are built in; other languages can be added via `projectile-test-at-point-rules`.

### Changes

- Project root detection and project-type detection now probe marker files with a single directory listing per directory level instead of one file stat per marker, collapsing dozens of sequential round-trips over TRAMP into one.
  - Marker matching is exact-case as a result, even on case-insensitive filesystems. This corrected the `gnumake` type's marker to GNU make's actual `GNUmakefile` spelling, and the `make` type now recognizes a lowercase `makefile` too.
- `projectile-auto-discover` now defaults to `t`, so setting `projectile-project-search-path` is enough to have those projects discovered (no change for anyone without a search path).
  - The scan now runs once per session on the first project switch, rather than on every switch, and remote (TRAMP) search-path entries are skipped.
- [#1771](https://github.com/bbatsov/projectile/issues/1771), [#740](https://github.com/bbatsov/projectile/issues/740): Hybrid indexing now applies the dirconfig glob patterns (`-`/`!` entries without a leading slash); previously they were silently ignored under `hybrid` and only `/`-prefixed path entries took effect.
- [#1941](https://github.com/bbatsov/projectile/issues/1941): Dirconfig glob patterns now follow `.gitignore`-like rules, identical under `native` and `hybrid` indexing (previously `native` used loose string suffixes, so `-build` also ignored `mybuild`, and expanded globs per directory level, so matching differed from level to level):
  - a slashless pattern matches the file name or any directory segment at any depth;
  - a pattern containing a slash is anchored at the project root (prefix with `**/` to match anywhere);
  - a trailing slash matches directories only, and a matched directory covers its subtree;
  - `*` stops at `/` while `**` crosses it.
- Remove `projectile-check-pattern-p` and `projectile-ignored-rel-p`, the old pattern matchers superseded by the compiled dirconfig matcher (nothing referenced them anymore).
- `projectile-verify-file` now goes through `projectile-file-exists-p`, so cold project-type detection benefits from the remote file-exists cache instead of issuing a TRAMP round-trip for every marker file probed.
- The mode-line updater is only added to `window-configuration-change-hook` when `projectile-dynamic-mode-line` is enabled; change the option via Customize or `setopt` for it to apply immediately.
- The `recentf` and `recently-active` sort orders no longer rescan the full project file list once per recent file, making them usable on very large projects.
- [#1953](https://github.com/bbatsov/projectile/issues/1953): Cache the git submodule listing instead of shelling out to `git submodule foreach` on every alien/hybrid file listing; the cache invalidates automatically when `.gitmodules` changes and is also cleared by `projectile-invalidate-cache`.
- User-facing conditions (no project found, missing optional package, nothing to toggle to, etc.) now signal `user-error` instead of `error`, so they no longer trigger the debugger under `debug-on-error`.

### Bugs fixed

- [#1115](https://github.com/bbatsov/projectile/issues/1115): `projectile-replace` no longer skips replacements (or reports "All files processed" without replacing anything) when the project root is in abbreviated `~/...` form, or when a match's case differs from a lower-case input.
- [#1677](https://github.com/bbatsov/projectile/issues/1677): `projectile-replace` and `projectile-replace-regexp` now scan buffers that already visit a project file from the beginning, so matches before point in those buffers are no longer missed.
- [#1849](https://github.com/bbatsov/projectile/issues/1849): `projectile-skel-dir-locals` now exits its variable-entry loop on an empty variable name, keeping the entries made so far, instead of trapping the user in the prompt and discarding input on `C-g`.
- CMake preset files referenced via `include` are now resolved relative to the file that includes them; previously the top-level `CMakePresets.json`'s includes resolved against `default-directory`, silently dropping the included presets when the current buffer was outside the project root.
- [#1600](https://github.com/bbatsov/projectile/issues/1600): The default git submodule listing no longer depends on a Unix shell (single quotes, `tr`), fixing alien/hybrid indexing of projects with submodules on Windows.
- Invalidating a project's cache now cancels its pending deferred cache flush; previously a flush scheduled before the invalidation could fire afterwards and recreate the just-deleted cache file with empty contents.
- Frecency tracking now works for projects reached through a symlinked root; the visited file is resolved the same way as the project root, so its visits are no longer silently dropped.
- `projectile-find-other-file` no longer treats the dot before a file's extension as a wildcard when matching, so `foo.el` won't match an unrelated `fooXel`-style name.

## 3.0.0 (2026-07-01)

### Changes

- Remove the dedicated `ido`/`ivy`/`helm` completion systems (and the `auto` mode that detected `ido-mode`/`ivy-mode`/`helm-mode`). `projectile-completing-read` now uses Emacs's `completing-read` (the former `default`), which works with Vertico, Consult, Fido, `ido-completing-read+`, etc., and still tags candidates with the `project-file` category. `projectile-completion-system` now takes `default` or a custom function; the removed values degrade to `default`, so nothing breaks. `helm-projectile` and `counsel-projectile` are unaffected (they don't go through this path).
- `projectile-find-references` now honours Projectile's ignore configuration (`.projectile` and the globally-ignored files/directories) and is scoped via the project's file set, like `projectile-grep`/`-ag`/`-ripgrep`. It previously went through the semantic-symref API, which searched the whole tree and ignored that configuration. It also now defaults the prompt from the active region (not just the symbol at point) and no longer carries a dead pre-27 display fallback. The search remains a backend-agnostic textual one; for semantic references use the built-in `xref-find-references` (which is also scoped to the Projectile project).
- Remove the built-in tags support: `projectile-find-tag`, `projectile-regenerate-tags`, `projectile-visit-project-tags-table`, and the `projectile-tags-command`/`projectile-tags-backend` options (along with the ggtags/etags-select special casing). ctags/etags navigation has been largely superseded by `xref` and LSP (`eglot` is built in since Emacs 29). Use `xref-find-definitions` directly, or `projectile-find-references` for project-wide references. `projectile-tags-file-name` is kept, since it's still used to exclude a generated tags file from indexing.
- Remove `projectile-browse-dirty-projects` and `projectile-vcs-dirty-state`. The implementation spun up `vc-dir` and busy-waited (`sleep-for`) up to 30 seconds per project across every known project, scraping status strings - slow, blocking, and niche. Use Magit, `vc-dir`, or a dedicated tool to find projects with uncommitted changes.
- Remove the idle timer (`projectile-enable-idle-timer`, `projectile-idle-timer-seconds`, `projectile-idle-timer-hook`). It existed mainly to re-run `projectile-regenerate-tags` on an idle timer, which makes little sense in an LSP/xref world, and it was off by default. Use a plain `run-with-idle-timer` if you really want this behavior.
- Remove `projectile-commander` (and `def-projectile-commander-method`), the single-key command dispatcher. It's superseded by `projectile-dispatch`, the `transient` menu added in this cycle. The `s-p m` binding and the `C-u s-p p` project-switch prefix now invoke `projectile-dispatch` instead (the prefix falls back to `projectile-switch-project-action` when `transient` isn't installed).
- A cold `projectile-find-file` (and any command that lists project files) no longer freezes Emacs while the project is indexed under the `alien`/`hybrid` methods. The indexing command runs asynchronously and is awaited in a way that keeps Emacs responsive to redisplay and `C-g` (which aborts the indexing), instead of blocking until it finishes. The resulting file list is identical; only the responsiveness during indexing differs. Controlled by the new `projectile-async-indexing` (default on); it has no effect under `native` indexing, in batch mode, or while a keyboard macro runs, all of which index synchronously as before.
- Speed up native indexing further: `projectile-index-directory` now reads each directory with `directory-files-and-attributes`, so an entry's type comes from the listing call instead of a `file-directory-p` stat per file. That stat was a separate filesystem round-trip each, which dominated the walk on large and remote (TRAMP) trees - it's now one round-trip per directory instead of one per file. Symlinks pointing at directories are still followed, matching the previous behaviour. Roughly 40% faster on a local 12k-file tree; the win is far larger over TRAMP.
- Speed up native indexing's post-walk step: `projectile-dir-files-native` strips the project-root prefix with a single `substring` per file instead of `file-relative-name`, which paid for an `expand-file-name`/`abbreviate-file-name` per file. `projectile-project-files` also skips re-relativising the listing when the only directory walked is the project root itself (the common single-directory case). Together that removes roughly two seconds of overhead per 90k files indexed.
- [#1872](https://github.com/bbatsov/projectile/issues/1872): Clarify in the `projectile-register-project-type` docstring and the manual that a list of `marker-files` must *all* be present for a type to match (logical AND), and that a predicate function should be used to match when any one of several files is present.
- [#1935](https://github.com/bbatsov/projectile/issues/1935): The `emacs-eask` project type now has a `test` command (`eask test`), so `projectile-test-project` works out of the box for Eask projects. Eask auto-detects the test framework (buttercup, ERT, ...), so no framework-specific type is needed.
- [#1638](https://github.com/bbatsov/projectile/issues/1638): Document in `projectile-svn-command` that it runs non-interactively and therefore needs SVN credentials to be cached up front for authenticated remotes.
- Keep a separate command history per lifecycle command type (configure, compile, test, install, package, run), so the prompt's `M-p` history no longer mixes, say, test commands with compile commands. `projectile-repeat-last-command` still uses the combined per-project history and is unaffected.
- Remove the unused private `projectile--init-known-projects` alias (a leftover compatibility shim for the old known-projects API; nothing in the codebase referenced it).
- `projectile-get-immediate-sub-projects` skips the `git submodule foreach` shell-out for git projects with no `.gitmodules` file anywhere up the parent chain. Hot path for monorepos that index the project root often.
- `projectile-discover-projects-in-directory` now uses `directory-files-no-dot-files-regexp` to skip `.` and `..` at the C level instead of doing the post-filter in Elisp - matches the indexing walker.
- Document the anchored vs `*`-prefixed semantics of `projectile-globally-ignored-directories`, the `find` fallback's lack of common directory exclusions when `fd` isn't available, and how `fd`/`git ls-files` handle deleted-but-unstaged files differently.
- Speed up native indexing on large trees: `projectile-index-directory` now hashes the ignored-files / ignored-directories / globally-ignored-directory-names lists once per indexing call (the per-file `member` scans were O(N*M)), expands dirconfig glob patterns once per directory level instead of once per (file, pattern) pair, and accumulates results into a shared cell so we no longer pay for an `apply append` at each recursion level.
- `projectile-remove-ignored` (hybrid post-processing) now hashes the ignored-files basenames and pre-splits ignored-dirs into prefix-match and any-segment groups, so the per-file inner loops drop from O(M) `seq-some` walks to O(1) hash lookups (or O(segments) for `*`-prefixed entries).
- Hybrid indexing now batches the external command into a single invocation when the project's `.projectile` declares multiple `+` keep entries, instead of shelling out once per kept subdirectory. The kept paths are passed to the indexing tool (e.g. `git ls-files`, `fd`, `find`) as positional pathspecs and submodule files outside those subdirectories are filtered out. Resolves the long-standing TODO in `projectile-project-files`.
- `projectile-files-via-ext-command` now accepts an optional `pathspecs` argument; entries are shell-quoted before being appended to the command. `projectile-dir-files-alien` similarly accepts an optional `subdirs` argument that threads through.
- Document the `hybrid` indexing method in the manual and add a feature matrix showing which Projectile knobs (dirconfig, global ignores/unignores, sort order, default caching) apply under `native`/`hybrid`/`alien`.
- `projectile-dir-files-alien` now accepts an optional `vcs` argument so the dispatcher can thread through the already-resolved VCS instead of recomputing it. Existing single-argument callers are unaffected.
- `projectile-index-directory` (native indexing) now relies on `directory-files-no-dot-files-regexp` to filter out `.` and `..` at the C level instead of walking past them in Elisp.
- `projectile-project-root-cache` now keys entries on cons cells (`(FUNC . DIR)` for per-function results, `('none . DIR)` for the overall failure marker) instead of formatted strings. This is internal state, but third-party code that reaches into the cache directly will need to update.
- `projectile-parse-dirconfig-file` now returns a `projectile-dirconfig` struct (with `keep`, `ignore`, `ensure`, and `prefixless-ignore` slots) instead of a positional 3-tuple. External callers should use the accessors (`projectile-dirconfig-keep` etc.) rather than `car`/`cadr`/`caddr`.
- Soft-deprecate prefix-less ignore entries in `.projectile`. Lines without a `+`/`-`/`!` prefix are still treated as ignore patterns for backward compatibility, but a one-time warning is now shown for each project that uses them. Set `projectile-warn-on-prefixless-dirconfig-lines` to nil to silence.
- **[Breaking]** Remove the legacy single-letter lifecycle keybindings `C`/`K`/`L`/`P`/`u` (configure/package/install/test/run project). Use the `c` prefix instead (`c o`, `c p`, `c i`, `c t`, `c r`).
- Remove two long-obsolete aliases: `projectile-global-mode` (use `projectile-mode`) and `projectile-project-root-files-functions` (use `projectile-project-root-functions`).
- Pre-compute file timestamps in `projectile-sort-by-modification-time` and `projectile-sort-by-access-time` to reduce stat calls from O(n log n) to O(n).
- Cache `projectile-parse-dirconfig-file` results per project root (invalidated by file modification time), avoiding redundant file reads during indexing.
- Cache `file-truename` results in `projectile-project-buffer-p` when checking multiple buffers, reducing redundant symlink resolution.
- Use a hash set for deleted file removal in `projectile-dir-files-alien`, improving performance from O(n*m) to O(n+m) when filtering deleted-but-unstaged files.
- Avoid redundant `projectile-project-root` call in `projectile-detect-project-type` by passing through the already-resolved root.
- Share `file-truename` cache across buffers in `projectile-open-projects`, matching the optimization already in `projectile-project-buffers`.
- Remove unnecessary temp buffer creation in `projectile--cache-project-commands-p` (was creating a buffer and re-reading `.dir-locals.el` on every compile/test/run invocation).
- **[Breaking]** Bump the minimum required Emacs version to 28.1 (from 26.1). This removes ~30 lines of compatibility code (fileloop fallback, `time-convert` fallback, `projectile-flatten` shim), makes `transient` (and therefore `projectile-dispatch`) an unconditional dependency, and fixes the `tags-query-replace` FIXME in `projectile-replace-regexp`.
- Add `compat` as a dependency, enabling the use of modern Emacs APIs (e.g. `string-replace`) on older Emacs versions.
- Replace most `cl-lib` sequence functions with `seq.el` equivalents (`seq-filter`, `seq-remove`, `seq-some`, `seq-find`, `seq-sort`, `seq-every-p`, `seq-difference`) and convert `cl-case` to `pcase`.
- [#1971](https://github.com/bbatsov/projectile/pull/1971): Support `slnx` files for dotnet project types.
- [#1958](https://github.com/bbatsov/projectile/issues/1958): Exclude `.projectile-cache.eld` from search results (ripgrep/ag/grep) by default.
- [#1947](https://github.com/bbatsov/projectile/issues/1947): `projectile-project-name` should be marked as safe.
- Set `projectile-auto-discover` to `nil` by default (to avoid startup slowdowns in some situations).
- [#1943](https://github.com/bbatsov/projectile/pull/1943): Consider `projectile-indexing-method` to be safe as a dir-local variable if it is one of the preset values.
- [#1936](https://github.com/bbatsov/projectile/issues/1936): Do not require selecting a project when using `M-x projectile-invalidate-cache`, since there is a global cache that is also cleared by that command, even when not operating on any specific project.
- Add `build.mill` as an alternative project marker for the Mill project type, matching Mill's current recommended file extension.
- Replace obsolete `when-let` and `cl-gensym` with `when-let*` and `gensym` for compatibility with Emacs 31+.

### New features

- [#1697](https://github.com/bbatsov/projectile/issues/1697): Add `projectile-add-and-switch-project`, which prompts for a directory, adds it to the known projects, and immediately switches to it.
- [#1698](https://github.com/bbatsov/projectile/issues/1698): Support `%p` in project command strings (configure/compile/test/run/install/package); it is replaced with the project name when the command runs, so e.g. `:run "docker run %p"` works for both registered project types and interactively entered commands.
- Add `projectile-run` (`s-p x r`), a single command that opens a shell, REPL or terminal in the project root over a *pluggable backend* (the same registry that powers `projectile-search`). Projectile ships `shell`, `eshell`, `ielm`, `term`, `vterm`, `eat` and `ghostel` backends; `projectile-shell-backend` selects which is used (default `eshell`). Register your own terminal with `projectile-register-shell-backend`. The dedicated commands (`projectile-run-eshell`, `projectile-run-vterm`, ..., and the `-other-window` variants) are kept as thin wrappers that force a backend.
- Add `projectile-search` (`s-p s s`), a single search command that runs over a *pluggable backend*. Projectile ships `grep`, `ripgrep` and `ag` backends; `projectile-search-backend` selects which is used (`auto` by default, favouring ripgrep then grep). Register your own tool (deadgrep, consult-ripgrep, ...) with `projectile-register-search-backend`. `projectile-grep`, `projectile-ripgrep` and `projectile-ag` are kept as thin wrappers that force a specific backend. Note the keymap change: `s-p s s` is now `projectile-search` (it used to be `projectile-ag`), and `projectile-ag` moves to `s-p s a`. The generic registry behind this (`projectile-register-backend`) is designed to be reused for other command families later.
- `projectile-dispatch` now exposes command modifiers as `transient` switches. A *Modifiers* group offers `--invalidate-cache` (`-i`, rebuild the file cache first, for the find file/dir commands), `--regexp` (`-r`, for `ag`/`ripgrep`), `--new-process` (`-n`, for the shells/REPLs), and `--display` (`-d`, cycle the display target through this window / other window / other frame, for the file/buffer/project commands). The `--display` switch replaces the old dedicated "Other window" and "Other frame" menu columns. (The `projectile-command-map` `4 <key>` / `5 <key>` bindings are unchanged.)
- Add `projectile-consult.el`, an optional Consult integration distributed alongside Projectile (with a soft dependency on `consult`, so it's only loaded if you `require` it). `projectile-consult-find-file` drives a streaming Consult finder from Projectile's own indexing command (via `projectile-project-files-producer`), so candidates appear in the minibuffer as the project is indexed instead of blocking until it's done, while still honouring the project's VCS and indexing configuration. The module is self-contained so it can be split into its own package later; it requires Emacs 29.1+ (Consult's floor).
- Add `projectile-index-project-async`, which indexes the current project in the background (via `make-process`) and populates the files cache without freezing Emacs, so a later `projectile-find-file` finds the cache already warm. Works with the `alien`/`hybrid` indexing methods (the `native` Elisp walk can't run off the main thread) and requires caching to be enabled. The underlying building blocks - `projectile-files-via-ext-command-async` and `projectile-dir-files-alien-async` - are public, so a streaming finder (e.g. one built on `consult`) can drive Projectile's indexing command asynchronously. Remote projects are handled via TRAMP. The result is identical to the synchronous indexing path.
- Add `projectile-project-files-producer`, which returns a plist (`:directory`, `:vcs`, `:command`, `:separator`) describing how to list a project's files with its external indexing command. It gives an external asynchronous/streaming file finder (e.g. one built on `consult` or `affe`) a single, stable entry point for driving Projectile's own indexing command instead of stitching together the root, VCS and command builders by hand.
- [#1956](https://github.com/bbatsov/projectile/issues/1956): Add `projectile-switch-project-other-window` and `projectile-switch-project-other-frame` (bound to `4 p` and `5 p`), which switch to a project and display it in another window/frame. The action they run is configurable via `projectile-switch-project-other-window-action` / `projectile-switch-project-other-frame-action` (find-file in the other window/frame by default).
- [#1809](https://github.com/bbatsov/projectile/issues/1809): Track the project switched away from in `projectile-most-recent-project` and add `projectile-switch-to-most-recent-project` to jump back to it (repeated calls toggle between the two). Only switches made through Projectile's switch-project commands are tracked.
- [#1861](https://github.com/bbatsov/projectile/issues/1861): Add `projectile-discard-command-cache` to drop the cached configure/compile/test/install/package/run commands for the current project, so a freshly edited `.dir-locals.el` (or project-type default) is picked up on the next run. Can be called manually or added to `after-save-hook`.
- Add `projectile-uniquify-dirname-transform`, a project-aware value for `uniquify-dirname-transform` that disambiguates same-named buffers using the project name. Mirrors `project.el`'s `project-uniquify-dirname-transform`.
- Add `projectile-dispatch`, a `transient` menu mirroring `projectile-command-map` for more discoverable command access. `transient` is an optional dependency (it requires Emacs 28+); the menu is only available when it's installed and is not bound to a key by default.
- [#2008](https://github.com/bbatsov/projectile/issues/2008): Add `projectile-remove-project-type` to unregister a project type. This is the supported way to stop Projectile auto-detecting a type; clearing its `marker-files` does not work (see the related bug fix below).
- [#1936](https://github.com/bbatsov/projectile/issues/1936): Add `projectile-discard-root-cache` command to clear `projectile-project-root-cache` without touching other Projectile caches. Useful after creating or removing a project marker, since the existing `projectile-invalidate-cache` either also drops the file list cache or prompts for a project depending on context.
- Warn once per session when `projectile-indexing-method` is `alien` but the project has a non-empty `.projectile` file, so users notice their dirconfig rules are being bypassed. Controlled by the new `projectile-warn-when-dirconfig-is-ignored` option.
- Warn when a `+` keep entry in `.projectile` contains glob metacharacters. The `+` prefix is for subdirectory paths only and globs are silently coerced into a non-matching directory name; the warning surfaces the misuse rather than letting it fail silently.
- [#1964](https://github.com/bbatsov/projectile/issues/1964): Implement `project-name` and `project-buffers` methods for the `project.el` integration, so that code using `project.el` APIs returns correct results for Projectile-managed projects.
- Implement the `project-ignores` method for the `project.el` integration, translating Projectile's global ignores, ignored file suffixes, and dirconfig (`.projectile`) ignore entries into the glob format `project.el` expects. This makes Projectile a more complete `project.el` backend for tools that rely on the protocol (e.g. `project-find-regexp`).
- Add `projectile-forget-projects-under` to drop all known projects located under a directory (with a prefix argument it recurses into nested projects). Mirrors `project.el`'s `project-forget-projects-under`.
- Add `projectile-forget-zombie-projects` as an alias for `projectile-cleanup-known-projects`, for discoverability and parity with `project.el`'s `project-forget-zombie-projects`.
- `projectile-kill-buffers-filter` now also accepts a composable list of conditions (buffer-name regexps, predicates, and `major-mode`/`derived-mode`/`not`/`and`/`or` forms), modeled on `project.el`'s `project-kill-buffer-conditions`. The existing `kill-all`, `kill-only-files`, and predicate-function values keep working unchanged.
- [#1837](https://github.com/bbatsov/projectile/issues/1837): Add `eat` project terminal commands with keybindings `x x` and `x 4 x`.
- Add `ghostel` project terminal commands with keybindings `x G` and `x 4 G`.
- Add keybinding `A` (in the projectile command map) and a menu entry for `projectile-add-known-project`.
- [#1653](https://github.com/bbatsov/projectile/issues/1653): Add `projectile-compile-subproject` and `projectile-test-subproject` commands for building/testing individual modules in multi-module projects (e.g. Maven, Gradle). Bound to `c m c` and `c m t`.

### Bugs fixed

- [#2005](https://github.com/bbatsov/projectile/issues/2005): Fix hybrid indexing with `fd` when the `.projectile` file has `+` keep entries. The kept directories were appended to the `fd` command as bare positional arguments, but `fd`'s grammar is `[pattern] [path...]` (so the first path was misread as the search pattern) and `fd` 9+ additionally rejects `--strip-cwd-prefix` alongside explicit paths (`error: ... '--strip-cwd-prefix' cannot be used with '[path]...'`). Projectile now passes the directories to `fd` via `--search-path` and drops `--strip-cwd-prefix` in that case. Other tools (`git ls-files`, `find`, etc.) still get plain trailing paths.
- [#2042](https://github.com/bbatsov/projectile/issues/2042): Asynchronous indexing now runs its command under `/bin/sh` rather than the user's interactive `shell-file-name`. The async runner wraps the command as `{ ...; } 2>file`, which is POSIX-sh syntax and broke for users whose shell is `csh`/`tcsh`/`fish` (it produced bogus `{`-related output instead of a file list). The indexing command itself is a plain POSIX command, so a POSIX shell is the correct interpreter regardless of the login shell.
- [#2046](https://github.com/bbatsov/projectile/issues/2046): Commands chosen from the `projectile-dispatch` menu now run in the project that was just selected, instead of the current buffer's project. The menu is a `transient`, so its suffix commands run after the project switch has unwound; the switched-to project is now kept current for the lifetime of the menu and restored when it exits. This applies whether the menu is reached via `C-u s-p p` or by setting `projectile-switch-project-action` to `projectile-dispatch` (or any other transient).
- [#2037](https://github.com/bbatsov/projectile/issues/2037): `projectile-consult.el` now byte-compiles cleanly when `consult` isn't installed. Because the module ships inside the Projectile MELPA package, build systems (package.el, nixpkgs) try to compile it without `consult` present, which previously errored on its top-level `(require 'consult)`. `consult` is now loaded softly at the top (and hard-required inside the command, where it's actually needed), with the used functions forward-declared.
- [#2042](https://github.com/bbatsov/projectile/issues/2042): A non-zero exit from the indexing command (`fd`, `git ls-files`, `find`, ...) no longer aborts `projectile-find-file` when the command still produced a file listing. External listers like `fd` routinely exit non-zero on benign conditions (e.g. an unreadable directory hit mid-traversal); that output is now used. A `user-error` is still raised when a non-zero exit produced no output at all, so a genuinely broken/missing command is still surfaced rather than mistaken for an empty project.
- [#1663](https://github.com/bbatsov/projectile/issues/1663): Projects matched by `projectile-ignored-projects` (or `projectile-ignored-project-function`) are now excluded from `projectile-relevant-known-projects`, so they no longer show up in `projectile-switch-project` even when they were added to the known projects before being ignored. Previously the ignore list was only consulted when adding a project.
- [#1909](https://github.com/bbatsov/projectile/issues/1909): A project type registered with a predicate `marker-files` function now receives the project root as its argument when detecting the current project's type. Previously it was passed `nil`, so such a function could never match the current project.
- [#1829](https://github.com/bbatsov/projectile/issues/1829): `projectile-project-root` no longer errors with `(wrong-type-argument stringp nil)` when `default-directory` is nil (which can happen in some non-file buffers); it returns nil instead.
- [#1946](https://github.com/bbatsov/projectile/issues/1946): `projectile-ripgrep` now builds its ignore exclusions as `--glob=!PATTERN` instead of `--glob '!PATTERN'`. The surrounding single quotes were only stripped by POSIX shells, so on Windows `cmd` they became part of the pattern and the exclusions silently failed.
- [#2008](https://github.com/bbatsov/projectile/issues/2008): A project type with an empty `marker-files` list no longer matches every project. `projectile-verify-files` is vacuously true for an empty list, so a type whose markers were cleared (e.g. via `projectile-update-project-type ... :marker-files nil`) would be detected everywhere instead of nowhere. `projectile-detect-project-type` now treats an empty marker set as a non-match.
- `projectile-discard-root-cache` and `projectile-invalidate-cache` now also clear `projectile-file-exists-cache`. Without this, after creating a new project marker (`.projectile`, `.git`, etc.) over TRAMP, the negative entries cached during earlier root-walks would keep reporting "not found" for up to `projectile-file-exists-remote-cache-expire` seconds even after the user explicitly invalidated the root cache.
- `projectile-find-file-hook-function` no longer disables *all* of Projectile for remote buffers - the cheap operations (file caching, known-projects tracking, project buffer-count cap) now run regardless of remoteness, and only the genuinely slow ones (mode-line update, tags-table visit) stay gated. Previously the entire hook was a single `(unless (file-remote-p ...))`, which meant remote projects never showed up in known-projects auto-tracking and never had their files cached on visit.
- Stop stat'ing remote known projects in `projectile-keep-project-p`. The "remote and connected" branch used `file-readable-p`, which is a remote round-trip per project; with `projectile-auto-cleanup-known-projects` enabled, that turned every project switch into a serial network walk over all remote known projects. Remote projects are now always kept; users can drop dead ones with `projectile-remove-known-project`.
- Skip `file-truename` for remote paths in `projectile-ignored-project-p` and `projectile-ignored-projects`. The known-projects tracker calls `projectile-ignored-project-p` from the find-file hook for every visit, and the previous behavior round-tripped to the remote on each call (and once per ignored entry) just to canonicalize symlinks.
- `projectile-project-buffer-p` now skips the `file-truename` call for buffers visiting remote (TRAMP) files. Each truename was a remote round-trip and `projectile-project-buffers` iterates the full `buffer-list`, so projects with N remote buffers in distinct directories were paying N round-trips on every call. Resolving symlinks across a TRAMP boundary is rarely meaningful in any case.
- Replace the 10 sequential `file-exists-p` probes in `projectile-project-vcs` with a single `directory-files` listing, and cache the per-project result in `projectile-project-vcs-cache`. Also collapse the up-the-tree dominator walk so each ancestor directory is listed at most once instead of up to 10 times. Over TRAMP this turns "1 round-trip per VCS marker per probed directory" into "1 round-trip per probed directory", and the cache means follow-up callers (the indexing path, `projectile-project-info`, the project-cleanup loop) hit memory.
- Cut down `projectile-project-root` round-trips on TRAMP. The cache-validity check used raw `file-exists-p` on the cached project root, which is a fresh remote stat on every call; switched to `projectile-file-exists-p` so the per-host TTL applies. Also memoize `(file-truename dir)` across the `projectile-project-root-functions` loop so cache-miss calls compute the truename once instead of once per probe (4-5 remote stats with the default function list).
- [#1501](https://github.com/bbatsov/projectile/issues/1501), [#1275](https://github.com/bbatsov/projectile/issues/1275): Detect `fd`/`fdfind` on the remote host for TRAMP projects instead of reusing the locally-detected `projectile-fd-executable`. The locally-resolved value is meaningless on the remote, so when `fd` was installed on the local box but not on the remote (or vice-versa), git indexing would silently fall back to an empty file list. The new `projectile-fd-executable-for` returns the per-directory executable, with a per-host cache so the remote `executable-find` lookup runs at most once per host per session.
- [#1898](https://github.com/bbatsov/projectile/issues/1898), [#1932](https://github.com/bbatsov/projectile/issues/1932): Surface non-zero exit codes from the indexing command in `projectile-files-via-ext-command` instead of silently returning an empty file list. The most common cause is `fd`/`git` being installed locally but not on the remote host of a TRAMP project — previously this manifested as an unexplained empty completion list (or `stringp, nil` crashes downstream); it now signals a `user-error` pointing at the `*projectile-files-errors*` buffer for the captured stderr.
- [#1211](https://github.com/bbatsov/projectile/issues/1211): Fix file-local `projectile-project-root` overrides being ignored after the first buffer in a directory was visited. The cache used `(default-directory)` as part of the key but `projectile-root-local` reads a buffer-local variable, so two buffers in the same directory with different overrides got the first buffer's answer. Results from `projectile-root-local` are no longer cached.
- [#1836](https://github.com/bbatsov/projectile/issues/1836): Memoize per-function `nil` results in the project root cache. Previously, when an early entry in `projectile-project-root-functions` returned nil, the nil was indistinguishable from a missing cache entry, so the function was re-run on every `projectile-project-root` call. With many entries this could cost several full directory walks per call. A `'none` sentinel is now stored for unsuccessful entries.
- [#1508](https://github.com/bbatsov/projectile/issues/1508): Fix dirconfig parser silently treating lines as ignore patterns when the `+`/`-`/`!` prefix or the comment character is preceded by whitespace; leading spaces and tabs are now skipped before prefix dispatch.
- Fix `projectile-files-via-ext-command` executing empty string as shell command for non-git VCS sub-projects.
- Fix `projectile-select-files` crashing on filenames with regexp metacharacters by using `string-search` instead of `string-match`.
- Fix `projectile-find-references` using internal `xref--show-xrefs` API whose signature changed across Emacs versions.
- Fix `projectile-determine-find-tag-fn` falling back to `find-tag` which was removed in Emacs 29; now falls back to `xref-find-definitions`.
- Fix `projectile-files-to-ensure` expanding wildcards relative to the current buffer instead of the project root.
- Fix `projectile-ignored-project-p` failing to match abbreviated paths against truename-resolved ignored projects list.
- Fix `projectile-edit-dir-locals` saving partial `.dir-locals.el` content when the user aborts skeleton insertion with `C-g`.
- Fix `projectile--other-extension-files` sort comparator ignoring its second argument, producing undefined ordering; replaced with a stable partition.
- Fix `projectile-toggle-project-read-only` operating on the wrong buffer after `add-dir-local-variable` by wrapping in `save-selected-window`.
- Fix `projectile-cache-current-file` calling `projectile-project-root` twice instead of reusing the already-resolved value.
- Fix `projectile-cache-current-file` queueing one idle timer per opened file, each capturing a stale snapshot of the file list. With persistent caching, opening many files in a session would result in N redundant disk writes after Emacs went idle. A pending flush is now coalesced per project and reads the latest in-memory cache at fire time.
- Fix `projectile-load-project-cache` not recording a cache time, which combined with `projectile-files-cache-expire` made the TTL check immediately re-evict freshly loaded data — every call ended up re-reading the cache file from disk and the data was never reindexed. The cache file's mtime is now used to seed `projectile-projects-cache-time`.
- Fix `projectile-load-project-cache` storing nil in cache on corrupt/empty cache files, preventing future reload attempts.
- Fix `projectile-purge-dir-from-cache` only updating the in-memory cache; with persistent caching the purged directory's files would reappear on the next session. The on-disk cache is now updated as well, matching the behavior of `projectile-purge-file-from-cache`.
- `projectile-invalidate-cache` now deletes the persistent cache file instead of overwriting it with a serialized `nil`.
- Fix `projectile--cmake-command-presets` using `mapcar` instead of `mapcan`, producing nested lists for included presets.
- Fix `projectile--eat` ignoring the `new-process` argument when generating buffer names.
- Fix `projectile-check-vcs-status` hanging indefinitely by adding a 30-second timeout to its busy-wait loop.
- Fix `projectile-sort-by-modification-time` and `projectile-sort-by-access-time` crashing on deleted files (nil `file-attributes`).
- Fix `projectile-recentf-files` failing to match files when the project root contains symlinks.
- Fix `projectile--run-project-cmd` passing nil to `compile` when no command is configured and `compilation-read-command` is nil — now signals a clear `user-error`.
- Fix `projectile--merge-related-files-fns` using destructive `nconc` which could corrupt shared data and silently drop values when the existing list was nil.
- Fix `projectile-configure-command` format call passing an unused `compile-dir` argument.
- Fix `projectile-update-project-type` resetting the project type cache with wrong hash table `:test` (used default `eql` instead of `equal` for string keys).
- Fix `projectile-find-file-hook-function` running `projectile-maybe-limit-project-file-buffers` on TRAMP buffers, causing potential hangs on slow connections.
- Use `expand-file-name` instead of `file-truename` in `projectile-compilation-dir` to avoid unnecessary symlink resolution (and TRAMP network round-trips).
- Use non-destructive `append` instead of `nconc` in `projectile-get-all-sub-projects` to avoid mutating caller data.
- Add `safe-local-variable` predicates for project settings variables (`projectile-project-test-suffix`, `projectile-project-compilation-cmd`, etc.) so they can be set in `.dir-locals.el` without prompting.
- [#1962](https://github.com/bbatsov/projectile/issues/1962): Fix `projectile-get-other-files` crashing when a candidate other-file lives in the project root directory.
- [#1816](https://github.com/bbatsov/projectile/issues/1816): Fix `projectile-expand-file-name-wildcard` failing when a parent directory is not readable (e.g. iCloud Drive, Termux).
- [#1841](https://github.com/bbatsov/projectile/issues/1841): Preserve user's `compilation-buffer-name-function` when `projectile-per-project-compilation-buffer` is nil.
- [#1823](https://github.com/bbatsov/projectile/issues/1823): Update the mode-line via `window-configuration-change-hook` so non-file buffers (e.g. Magit) display the correct project info.
- [#1886](https://github.com/bbatsov/projectile/issues/1886): Fix `(wrong-type-argument stringp nil)` error when running project commands in a newly created project by using `projectile-acquire-root` instead of `projectile-project-root` in `projectile--run-project-cmd`.
- [#1456](https://github.com/bbatsov/projectile/issues/1456): Fix `projectile-replace-regexp` stopping when encountering a missing file by filtering nonexistent files from the replacement file list.
- [#1687](https://github.com/bbatsov/projectile/issues/1687): Fix `projectile-grep-finished-hook` not running on subsequent grep calls due to buffer rename collision.
- [#1939](https://github.com/bbatsov/projectile/issues/1939): Handle corrupted `projectile-known-projects-file` gracefully instead of crashing with `(wrong-type-argument listp ...)`.
- [#1748](https://github.com/bbatsov/projectile/issues/1748): Fix `projectile-replace` falling back to the legacy Emacs 25/26 code path on Emacs 27+ because `fileloop` was not loaded.
- [#1741](https://github.com/bbatsov/projectile/issues/1741): Fix `projectile-replace` treating the search string as a regexp instead of a literal string on Emacs 27+.
- [#1729](https://github.com/bbatsov/projectile/issues/1729): Fix `projectile-root-top-down` to actually return the topmost matching project root instead of the bottommost.
- [#1596](https://github.com/bbatsov/projectile/issues/1596): `projectile-find-dir` now includes intermediate directories that contain only subdirectories (e.g. `src/` when it only has `src/ComponentA/`, `src/ComponentB/`).
- [#1551](https://github.com/bbatsov/projectile/issues/1551): Don't add nonexistent files to the project cache (e.g. when visiting a new file with `find-file` and then abandoning the buffer).
- [#1554](https://github.com/bbatsov/projectile/issues/1554): Fix `projectile-files-with-string` failing on special characters when using `grep` or `git-grep` by adding the `-F` (fixed-string) flag.
- [#1897](https://github.com/bbatsov/projectile/issues/1897): Filter deleted-but-unstaged files from `git ls-files` output in alien/hybrid indexing (when `fd` is not used).
- [#1873](https://github.com/bbatsov/projectile/issues/1873): Skip unreadable directories during native indexing instead of aborting with a permission error.
- [#1961](https://github.com/bbatsov/projectile/issues/1961): Prevent directories from matching file-type project root markers (e.g., a `workspace` directory no longer matches the `WORKSPACE` Bazel marker on case-insensitive filesystems).
- [#1749](https://github.com/bbatsov/projectile/issues/1749): Strip `./` prefix from `fd` output in `projectile-files-via-ext-command`, fixing compatibility with older `fd` versions that don't support `--strip-cwd-prefix`.
- Fix `projectile-ripgrep` failing on zsh when ignored file patterns contain glob characters, by quoting `--glob` arguments.
- Fix CMake version parsing failing when `cmake --version` output contains extra text after the version number.
- Fix Jujutsu file listing to use template syntax for null-byte-separated output, making it robust against user customization of `jj` output format.
- Fix `projectile-purge-file-from-cache` serializing the stale file list to disk instead of the updated one.
- Fix misplaced paren in `projectile-project-buffers-other-buffer` causing `switch-to-buffer` arguments to be ignored.
- Fix `projectile-default-generic-command` silently dropping lambda/closure commands (only symbol commands worked).
- Fix `dired-before-readin-hook` being added as buffer-local instead of global in `projectile-mode`, so it now correctly fires in all dired buffers.
- Fix `projectile-find-dir-hook` not being cleaned up when disabling `projectile-mode`.
- Use `display-warning` instead of `message` when cache serialization fails, making the failure visible in the `*Warnings*` buffer.

## 2.9.1 (2025-02-13)

### Bugs Fixed

- [#1929](https://github.com/bbatsov/projectile/pull/1929): Don't create cache files when `projectile-use-caching` is not set to `persistent`.

## 2.9.0 (2025-02-12)

### New features

- [#1870](https://github.com/bbatsov/projectile/pull/1870): Add package command for CMake projects.
- [#1875](https://github.com/bbatsov/projectile/pull/1875): Add support for Sapling VCS.
- [#1876](https://github.com/bbatsov/projectile/pull/1876): Add support for Jujutsu VCS.
- [#1877](https://github.com/bbatsov/projectile/pull/1877): Add custom variable `projectile-cmd-hist-ignoredups`.
- Add support for Eask projects.
- [#1892](https://github.com/bbatsov/projectile/pull/1892): Add category metadata to `completing-read`. (it's used by packages like `marginalia` and `embark`)
- [#1899](https://github.com/bbatsov/projectile/pull/1899): Add support for xmake build utility.
- [#1918](https://github.com/bbatsov/projectile/pull/1895): Add Zig project discovery.
- Add support for Swift project discovery.
- Introduce `projectile-global-ignore-file-patterns` config that allows to ignore files and directories with regexp patterns.
- Introduce `projectile-auto-cleanup-known-projects` option that allows you to auto-cleanup missing projects.

### Bugs fixed

- [#1881](https://github.com/bbatsov/projectile/issues/1881): Fix `projectile-recentf` when called outside any project.
- [#1915](https://github.com/bbatsov/projectile/pull/1915): Fix dotnet-sln project-type recognition. (check `*.sln` files instead of `src/`)
- [#1850](https://github.com/bbatsov/projectile/issues/1850): Ensure the presence of a project in `projectile-compilation-dir`.
- [#1811](https://github.com/bbatsov/projectile/issues/1811): Revert a change to `projectile-ignored-directories` that had converted them into regular expressions.
- [#1893](https://github.com/bbatsov/projectile/issues/1893): Fix `projectile-discover-projects-in-directory` when called interactively.

### Changes

- [#1874](https://github.com/bbatsov/projectile/pull/1874): Changes `compilation-find-file-projectile-find-compilation-buffer` to navigate directly to the file if already present on disk to help improve performance in scenarios where there are a large number of project directories.
- Drop support for Emacs 25.
- Rework the caching logic. The main changes from before are:

  - Each project has its own cache file
  - Cache files are consulted only when you request the files of some project

    This makes caching both more robust and faster, as before the cache file
    for all projects was loaded when projectile-mode was enabled.
- Make the cache transient by default. (meaning it lives only in memory and is not persisted to a file)
  - To enable persistent caching you need to set `projectile-enable-caching` to `'persistent`.
- Speed-up load time by moving known projects initialization outside of `projectile-mode`'s init.
  - As a side effect the known projects will be initialized properly even if you're not using `projectile-mode`.
  - The projects are read from disk the first time you invoke `projectile-switch-project` or a similar command.
- Introduce a common prefix for project lifecycle command keybindings:
  - `c o` -> `projectile-configure-project`
  - `c c` -> `projectile-compile-project`
  - `c p` -> `projectile-package-project`
  - `c i` -> `projectile-install-project`
  - `c t` -> `projectile-test-project`
  - `c r` -> `projectile-run-project`
  - The old keybindings will be removed in a future version of Projectile.

## 2.8.0 (2023-10-13)

### New features

- [#1862](https://github.com/bbatsov/projectile/pull/1862): Add project types "yarn" and "pnpm" separate from "npm".
- [#1851](https://github.com/bbatsov/projectile/pull/1851): Add ripgrep to `projectile-commander` with binding `?p`.
- [#1833](https://github.com/bbatsov/projectile/pull/1833): Add Julia project discovery.
- [#1828](https://github.com/bbatsov/projectile/pull/1828): Add Nimble-based Nim project discovery.
- Add elm project type.
- [#1821](https://github.com/bbatsov/projectile/pull/1821): Add `pyproject.toml` discovery for python projects.
- [#1830](https://github.com/bbatsov/projectile/issues/1830): Add command `projectile-run-vterm-other-window` and bind it to `x 4 v`.

### Changes

- [#1839](https://github.com/bbatsov/projectile/issues/1839): Ensure `projectile-toggle-between-implementation-and-test` also obeys `projectile-project-test-dir` and `projectile-project-src-dir`.
- [#1285](https://github.com/bbatsov/projectile/pull/1825): By default, use [fd](https://github.com/sharkdp/fd) in Git repositories instead of `git ls-files` when it is installed, in order to solve the problem where deleted files were still shown in `projectile-find-file` until their deletions were staged. The user-facing behavior should be the same, although potentially with different performance characteristics in large Git repositories. The old behavior can be reclaimed by setting `projectile-git-use-fd` to nil.
- [#1831](https://github.com/bbatsov/projectile/issues/1831): Enable the project.el integration only when `projectile-mode` is active.
- [#1847](https://github.com/bbatsov/projectile/issues/1847): Use literal directory name casing when toggling between impl and test.

### Bugs fixed

- Fix `fd` inserting color control sequences when used over tramp.
- [#1835](https://github.com/bbatsov/projectile/issues/1835): Reopening existing vterm buffer in other window
- [#1865](https://github.com/bbatsov/projectile/pull/1865): `projectile-generic-command` should use `projectile-fd-executable` to find the path for fd.

## 2.7.0 (2022-11-22)

### New features

- [#1591](https://github.com/bbatsov/projectile/issues/1591): Add `project.el` integration that will make Projectile the default provider for project lookup.
- Add new command `projectile-find-references` (bound to `C-c C-p ?` and `C-c C-p s x`).
- [#1737](https://github.com/bbatsov/projectile/pull/1737): Add helpers for `dir-local-variables` for 3rd party use. Functions `projectile-add-dir-local-variable` and `projectile-delete-dir-local-variable` wrap their built-in counterparts. They always use `.dir-locals.el` from the root of the current Projectile project.
- Add a new defcustom (`projectile-dirconfig-file`) controlling the name of the file used as Projectile’s root marker and configuration file.
- [#1813](https://github.com/bbatsov/projectile/pull/1813): Allow project-files to contain wildcards and allow multiple project-files per project type registration. Add a new project-type for .NET solutions.

### Changes

- [#1812](https://github.com/bbatsov/projectile/pull/1812): Add a `projectile-root-marked` function for finding roots marked by `.projectile`. Prioritize `.projectile` above other bottom-up root files.

### Bugs fixed

- [#1796](https://github.com/bbatsov/projectile/issues/1796): Fix `projectile-root-bottom-up` doesn't always find bottom-most file.
- [#1799](https://github.com/bbatsov/projectile/pull/1799): Fix `projectile-open-projects` lists projects for which all buffers are closed.
- [#1806](https://github.com/bbatsov/projectile/pull/1806): Fix `projectile-project-type` to return the correct project type even when we pass it the DIR arg. As a result of the fix,
`projectile-expand-root`, `projectile-detect-project-type`, `projectile-verify-files` , `projectile-verify-file` `projectile-verify-file-wildcard`, `projectile-cabal-project-p`,
`projectile-dotnet-project-p`, `projectile-go-project-p` and the newly factored out `projectile-eldev-project-p` now also takes an &optional DIR arg to specify the directory it is acting on.

## 2.6.0 (2022-10-25)

### New features

- [#1790](https://github.com/bbatsov/projectile/pull/1790): Add `src-dir` and `test-dir` properties for the mill project type.
- [#1778](https://github.com/bbatsov/projectile/pull/1778): Allow `projectile-replace` to select file extensions when using prefix arg (`C-u`).
- [#1757](https://github.com/bbatsov/projectile/pull/1757): Add support for the Pijul VCS.
- [#1745](https://github.com/bbatsov/projectile/pull/1745): Allow `projectile-update-project-type` to change project type precedence and remove project options.
- [#1699](https://github.com/bbatsov/projectile/pull/1699): `projectile-ripgrep` now supports [rg.el](https://github.com/dajva/rg.el).
- [#1712](https://github.com/bbatsov/projectile/issues/1712): Make it possible to hide Projectile's menu. See `projectile-show-menu`.
- [#1718](https://github.com/bbatsov/projectile/issues/1718): Add a project type definition for `GNUMakefile`.
- [#1747](https://github.com/bbatsov/projectile/pull/1747): Add support for preset-based install-commands for CMake projects.
- [#1768](https://github.com/bbatsov/projectile/pull/1768): Add support for disabling command caching on a per-project basis.
- [#1797](https://github.com/bbatsov/projectile/pull/1797): Make all project type attributes locally overridable.
- [#1803](https://github.com/bbatsov/projectile/pull/1803): Add support go-task/task.

### Bugs fixed

- [#1781](https://github.com/bbatsov/projectile/pull/1781): Fix `rails-rspec` and `rails-test` to use `app` instead of `lib` as `src-dir`.
- [#1762](https://github.com/bbatsov/projectile/pull/1762): Fix `projectile-globally-ignored-directories` unescaped regex.
- [#1713](https://github.com/bbatsov/projectile/issues/1731): Fix `projectile-discover-projects-in-directory` reordering known projects.
- [#1514](https://github.com/bbatsov/projectile/issues/1514): Fix `projectile-ag` global ignores not in effect.
- [#1714](https://github.com/bbatsov/projectile/issues/1714): Fix `projectile-discover-projects-in-directory` not interactive.
- [#1734](https://github.com/bbatsov/projectile/pull/1734): Make `projectile--find-matching-test` use `src-dir/test-dir` properties.
- [#1750](https://github.com/bbatsov/projectile/issues/1750): Fix source and test directories for Maven projects.
- [#1765](https://github.com/bbatsov/projectile/issues/1765): Fix `src-dir`/`test-dir` not defaulting to `"src/"` and `"test/"` with `projectile-toggle-between-implementation-and-test`.
- Fix version extraction logic.
- [1654](https://github.com/bbatsov/projectile/issues/1654) Fix consecutive duplicates appearing in command history.
- [#1755](https://github.com/bbatsov/projectile/issues/1755) Cache failure to find project root.

### Changes

- [#1785](https://github.com/bbatsov/projectile/pull/1785): Give the project type "go" higher precedence than universal types, namely "make".
- [#1447](https://github.com/bbatsov/projectile/issues/1447): Restructure the menu.
- [#1692](https://github.com/bbatsov/projectile/issues/1692): Enable minibuffer completions when reading shell-commands.
- Change the Grails project marker to `application.yml`.
- [#1789](https://github.com/bbatsov/projectile/pull/1789): Progress reporter for recursive progress discovery.
- [#1708](https://github.com/bbatsov/projectile/issues/1708): `projectile-ripgrep` now consistently searches hidden files.

## 2.5.0 (2021-08-10)

### New features

- [#1680](https://github.com/bbatsov/projectile/pull/1680): Add support for recursive project discovery.
- [#1671](https://github.com/bbatsov/projectile/pull/1671)/[#1679](https://github.com/bbatsov/projectile/pull/1679): Allow the `:test-dir` and `:src-dir` options of a project to be set to functions for more flexible test switching.
- [#1672](https://github.com/bbatsov/projectile/pull/1672): Add `projectile-<cmd>-use-comint-mode` variables (where `<cmd>` is `configure`, `compile`, `test`, `install`, `package`, or `run`). These enable interactive compilation buffers.
- [#1705](https://github.com/bbatsov/projectile/pull/1705): Add project detection for Nix flakes.

### Bugs fixed

- [#1550](https://github.com/bbatsov/projectile/issues/1550): Make `projectile-regenerate-tags` tramp-aware.
- [#1673](https://github.com/bbatsov/projectile/issues/1673): Fix CMake system-preset filename.
- [#1691](https://github.com/bbatsov/projectile/pull/1691): Fix `compilation-find-file` advice handling of directory.

### Changes

- Remove `pkg-info` dependency.

## 2.4.0 (2021-05-27)

### New features

- Add `projectile-update-project-type` function for updating the properties of existing project types.
- [#1658](https://github.com/bbatsov/projectile/pull/1658): New command `projectile-reset-known-projects`.
- [#1656](https://github.com/bbatsov/projectile/pull/1656): Add support for CMake configure, build and test presets. Enabled by setting `projectile-cmake-presets` to non-nil, disabled by default.
- Add optional parameters to `projectile-run-shell-command-in-root` and `projectile-run-async-shell-command-in-root`

### Changes

- Add `project` param to `projectile-generate-process-name`.
- [#1608](https://github.com/bbatsov/projectile/pull/1608): Use rebar3 build system by default for Erlang projects.
- Rename `projectile-project-root-files-functions` to `projectile-project-root-functions`.
- [#1647](https://github.com/bbatsov/projectile/issues/1647): Use "-B" in the mvn commands to avoid ANSI coloring clutter in the compile buffer
- [#1657](https://github.com/bbatsov/projectile/pull/1657): Add project detection for Debian packaging directories.
- [#1656](https://github.com/bbatsov/projectile/pull/1656): CMake compilation-dir removed to accommodate preset support, commands adjusted to run from project-root, with "build" still being the default build-directory. The non-preset test-command now uses "cmake" with "--target test" instead of "ctest".

### Bugs fixed

- [#1639](https://github.com/bbatsov/projectile/pull/1639): Do not ask twice for project running ielm, term and vterm.
- [#1250](https://github.com/bbatsov/projectile/issues/1250): Fix `projectile-globally-ignored-directories` not working with native indexing.
- [#1438](https://github.com/bbatsov/projectile/pull/1438): Make sure `projectile-files-via-ext-command` returns files, not errors.
- [#1450](https://github.com/bbatsov/projectile/pull/1450): Call `switch-project-action` within project's temp buffer.
- [#1340](https://github.com/bbatsov/projectile/pull/1340): Fix remote projects being removed if TRAMP can't connect.
- [#1655](https://github.com/bbatsov/projectile/pull/1655): Fix `projectile-replace-regexp` searching the wrong files when called with prefix arg.
- [#1659](https://github.com/bbatsov/projectile/issues/1659): Fix `projectile-project-vcs` to work outside a project.
- [#1637](https://github.com/bbatsov/projectile/pull/1661): Integrate with savehist-mode.

## 2.3.0 (2020-11-27)

### New features

- [#1517](https://github.com/bbatsov/projectile/issues/1517): Add project-specific compilation buffers and only ask to save files in the project when compiling.
- New functions `projectile-acquire-root` and `projectile-process-current-project-buffers-current`
- New project commands `projectile-package-project`, `projectile-install-project`.
- [#1539](https://github.com/bbatsov/projectile/pull/1539): New defcustom `projectile-auto-discover` controlling whether to automatically discover projects in the search path when `projectile-mode` activates.
- Add [emacs-eldev](https://github.com/doublep/eldev) project type.
- Add Dart project type.
- [#1555](https://github.com/bbatsov/projectile/pull/1555): Add search with ripgrep.
- Add Python-poetry project type.
- [#1576](https://github.com/bbatsov/projectile/pull/1576): Add OCaml [Dune](https://github.com/ocaml/dune) project type.
- Add [Mill](http://www.lihaoyi.com/mill/) project type.
- Auto-detect completion system, supporting `ido`, `ivy`, `helm` and the default completion system.

### Changes

- [#1540](https://github.com/bbatsov/projectile/pull/1540): Add default `test-suffix` to Angular projects.
- Add a `:project-file` param to `projectile-register-project-type`.
- [#1588](https://github.com/bbatsov/projectile/pull/1588): Improve performance of `projectile-ibuffer` with many buffers not in project.
- [#1601](https://github.com/bbatsov/projectile/pull/1601): Implement separate compilation command history for each project.

### Bugs fixed

- [#1377](https://github.com/bbatsov/projectile/issues/1377): Fix `projectile-regenerate-tags` directory.

## 2.2.0 (2020-06-10)

### New features

- [#1523](https://github.com/bbatsov/projectile/issues/1523): Add a new defcustom (`projectile-max-file-buffer-count`) controlling how many opened file buffers should Projectile maintain per project.
- Optional support for comments in .projectile dirconfig files using `projectile-dirconfig-comment-prefix`.
- [#1497](https://github.com/bbatsov/projectile/pull/1497): New command `projectile-run-gdb` (<kbd>x g</kbd> in `projectile-command-map`).
- Add [Bazel](https://bazel.build) project type.

### Bugs fixed

- [#1503](https://github.com/bbatsov/projectile/pull/1503): Leave archive before searching for the project root.

### Changes

- [#1528](https://github.com/bbatsov/projectile/pull/1528): Improve massively the performance of native indexing (it's around 10x faster now).

## 2.1.0 (2020-02-04)

### New features

- [#1486](https://github.com/bbatsov/projectile/pull/1486) Allow `projectile-run-shell/eshell/term/vterm/ielm` to start extra processes if invoked with the prefix argument.
- New command `projectile-run-vterm` (<kbd>x v</kbd> in `projectile-command-map`).
- Add `related-files-fn` option to use custom function to find test/impl/other files.
- [#1019](https://github.com/bbatsov/projectile/issues/1019): Jump to a test named the same way but in a different directory.
- [#982](https://github.com/bbatsov/projectile/issues/982): Add heuristic for projectile-find-matching-test.
- Support a list of functions for `related-files-fn` options and helper functions.
- [#1405](https://github.com/bbatsov/projectile/pull/1405): Add Bloop Scala build server project detection.
- [#1418](https://github.com/bbatsov/projectile/pull/1418): The presence of a `go.mod` file implies a go project.
- [#1419](https://github.com/bbatsov/projectile/pull/1419): When possible, use [fd](https://github.com/sharkdp/fd) instead
of `find` to list the files of a non-VCS project. This should be much faster.

### Bugs fixed

- [#675](https://github.com/bbatsov/projectile/issues/675): Performance improvement for native project indexing strategy.
- [#97](https://github.com/bbatsov/projectile/issues/97): Respect `.projectile` ignores which are paths to files and patterns when using `projectile-grep`.
- [#1391](https://github.com/bbatsov/projectile/issues/1391): A `.cabal` sub-directory is no longer considered project indicator.
- [#1385](https://github.com/bbatsov/projectile/issues/1385): Update `projectile-replace` for Emacs 27.
- [#1432](https://github.com/bbatsov/projectile/issues/1432): Support .NET project.
- [#1270](https://github.com/bbatsov/projectile/issues/1270): Fix running commands that don't have a default value.
- [#1475](https://github.com/bbatsov/projectile/issues/1475): Fix directories being ignored with hybrid mode despite being explicitly unignored.
- [#1482](https://github.com/bbatsov/projectile/issues/1482): Run a separate grep buffer per project root.
- [#1488](https://github.com/bbatsov/projectile/issues/1488): Fix `projectile-find-file-in-directory` when in a subdir of `projectile-project-root`.

## 2.0.0 (2019-01-01)

### New features

- [#972](https://github.com/bbatsov/projectile/issues/972): Add toggle for project read only mode: `projectile-toggle-project-read-only`.
- New interactive command `projectile-run-ielm`.
- Add [crystal](https://crystal-lang.org) project type.
- [#850](https://github.com/bbatsov/projectile/issues/850): Make it possible to prompt for a project, when you're not in a project, instead of raising an error. (see `projectile-require-project-root`).
- [#1147](https://github.com/bbatsov/projectile/issues/1147): Introduce a new indexing method called `hybrid` which behaves like the old `alien`.
- [#896](https://github.com/bbatsov/projectile/issues/896) Add commands `projectile-previous-project-buffer` and
`projectile-next-project-buffer` to switch to other buffer in the project.
- [#1016](https://github.com/bbatsov/projectile/issues/1016): Add a new defcustom (`projectile-current-project-on-switch`) controlling what to do with the current project on switch.
- [#1233](https://github.com/bbatsov/projectile/issues/1233): Add a new defcustom (`projectile-kill-buffers-filter`) controlling which buffers are killed by `projectile-kill-buffers`.
- [#1279](https://github.com/bbatsov/projectile/issues/1279): Add command `projectile-repeat-last-command` to re-execute the last external command in a project.

### Changes

- **(Breaking)** [#1147](https://github.com/bbatsov/projectile/issues/1147): Remove any post-processing from the `alien` indexing method.
- Specify project path for `projectile-regenerate-tags`.
- Handle files with special characters in `projectile-get-other-files`.
- [#1260](https://github.com/bbatsov/projectile/pull/1260): ignored-*-p: Now they match against regular expressions.
- **(Breaking)** Remove the default prefix key (`C-c p`) for Projectile. Users now have to pick one themselves.
- Deprecate `projectile-keymap-prefix`.
- Avoid "No projects needed to be removed." messages in global mode.
- [#1278](https://github.com/bbatsov/projectile/issues/1278): Add default `test-suffix` to `npm` project.
- [#1285](https://github.com/bbatsov/projectile/pull/1285): Add default `test-suffix` to Python projects.
- [#1285](https://github.com/bbatsov/projectile/pull/1285): Add support for Pipenv-managed Python projects.
- [#1232](https://github.com/bbatsov/projectile/issues/1232): Stop evaluating code dynamically in the mode-line and switch to a simpler scheme where the mode-line is updated just once using `find-file-hook`.
- Make the mode line configurable via `projectile-dynamic-mode-line` and `projectile-mode-line-function`.
- [#1205](https://github.com/bbatsov/projectile/issues/1205): Check that project directory exists when switching projects.
- Move Projectile's menu out of the "Tools" menu.
- [API] **(Breaking)** Stop raising errors from `projectile-project-root` if not invoked within a project. Now it will simply return nil. Use it together with `projectile-ensure-project` to emulate the old behavior.

### Bugs fixed

- [#1315](https://github.com/bbatsov/projectile/issues/1315): Give preference to the project types that were registered last.
- [#1367](https://github.com/bbatsov/projectile/issues/1367): Fix the Makefile so that we can compile projectile - use `make`.

## 1.0.0 (2018-07-21)

### New Features

- [#1255](https://github.com/bbatsov/projectile/pull/1255): Add support for function symbols as project default commands
- [#1243](https://github.com/bbatsov/projectile/pull/1243): Add [angular](https://angular.io) project support.
- [#1228](https://github.com/bbatsov/projectile/pull/1228): Add support for a prefix argument to `projectile-vc`.
- [#1221](https://github.com/bbatsov/projectile/pull/1221): Modify Ruby and Elixir project settings.
- [#1175](https://github.com/bbatsov/projectile/pull/1175): Add a command `projectile-configure-command` for running a configuration for build systems that need that.
- [#1168](https://github.com/bbatsov/projectile/pull/1168): Add CMake and Meson project support.
- [#1159](https://github.com/bbatsov/projectile/pull/1159) Add [nix](http://nixos.org) project support.
- [#1166](https://github.com/bbatsov/projectile/pull/1166): Add `-other-frame` versions of commands that had `-other-window` versions.
- Consider Ensime configuration file as root marker, `.ensime`.
- [#1057](https://github.com/bbatsov/projectile/issues/1057): Make it possible to disable automatic project tracking via `projectile-track-known-projects-automatically`.
- Added ability to specify test files suffix and prefix at the project registration.
- [#1154](https://github.com/bbatsov/projectile/pull/1154) Use npm install instead of build.
- Added the ability to expire old files list caches via `projectile-projectile-files-cache-expire`.
- [#1204](https://github.com/bbatsov/projectile/pull/1204): `projectile-register-project-type` can now be used to customize the source and test directory via `:src-dir` and `:test-dir` for projects with custom needs (eg. maven).
- [#1240](https://github.com/bbatsov/projectile/pull/1240): Add some integration with ripgrep.
- Add `projectile-project-search-path`, which is auto-searched for projects when `projectile-mode` starts.
- Add `projectile-discover-projects-in-search-path` command which searches for projects in `projectile-project-search-path`.
- Auto-cleanup missing known-projects on `projectile-mode` start.

### Changes

- [#1213](https://github.com/bbatsov/projectile/pull/1213): Cache project root in non-file-backed buffers.
- [#1175](https://github.com/bbatsov/projectile/pull/1175): `projectile-register-project-type` can now set a default compilation directory for build systems that need to build out-of-tree (eg. meson).
- [#1175](https://github.com/bbatsov/projectile/pull/1175): `projectile-{test,run}-project` now run inside `(projectile-compilation-dir)`, just like `projectile-compile-project`.
- [#1175](https://github.com/bbatsov/projectile/pull/1175): `projectile-{test,run}-project` now stores the default command per directory instead of per project, just like `projectile-compile-project`.
- Cache the root of the current project to increase performance
- [#1129](https://github.com/bbatsov/projectile/pull/1129): Fix TRAMP issues.
- Add R DESCRIPTION file to `projectile-project-root-files`.
- Ignore backup files in `projectile-get-other-files`.
- Ignore Ensime cache directory, `.ensime_cache`.
- [#364](https://github.com/bbatsov/projectile/issues/364): `projectile-add-known-project` can now be used interactively.
- `projectile-mode` is now a global mode.
- `projectile-find-tag` now defaults to xref on Emacs 25.1+.
- Add relation between `.h` and `.cc` files in `projectile-other-file-alist`.
- Cache the name of the current project for mode-line display of the project name.
- [#1078](https://github.com/bbatsov/projectile/issues/1078): For projectile-grep/ag use default value like grep/rgrep/ag.
- Don't treat `package.json` as a project marker.
- [#987](https://github.com/bbatsov/projectile/issues/987): projectile-ag ignores ag-ignore-list when projectile-project-vcs is git
- [#1119](https://github.com/bbatsov/projectile/issues/1119): File search ignores non-root dirs if prefixed with "*"
- Treat members of `projectile-globally-ignored-file-suffixes` as file name suffixes (previously treated as file extensions).
- Ensure project roots are added as directory names to avoid near-duplicate projects, e.g. "~/project/" and "~/project".
- Don't autoload defcustoms.
- **(Breaking)** Require Emacs 25.1.
- Remove the support for grizzl.

### Bugs fixed

- [#1222](https://github.com/bbatsov/projectile/issues/1222): `projectile-configure-project` fails for generic project type
- [#1162](https://github.com/bbatsov/projectile/issues/1162): `projectile-ag` causes "Attempt to modify read-only object" error.
- [#1169](https://github.com/bbatsov/projectile/issues/1169): `projectile-compile-project` does not prompt for compilation command.
- [#1072](https://github.com/bbatsov/projectile/issues/1072): Create test files only within the project.
- [#1063](https://github.com/bbatsov/projectile/issues/1063): Support Fossil checkouts on Windows.
- [#1024](https://github.com/bbatsov/projectile/issues/1024): Do not cache ignored project files.
- [#1022](https://github.com/bbatsov/projectile/issues/1022): Scan for Fossil's checkout DB, not its config DB.
- [#1007](https://github.com/bbatsov/projectile/issues/1007): Make use of `projectile-go-function`.
- [#1011](https://github.com/bbatsov/projectile/issues/1011): Save project files before running project tests.
- [#1099](https://github.com/bbatsov/projectile/issues/1099): Fix the behaviour of `projectile-purge-dir-from-cache`.
- [#1067](https://github.com/bbatsov/projectile/issues/1067): Don't mess up `default-directory` after switching projects.
- [#1246](https://github.com/bbatsov/projectile/issues/1246): Don't blow away real project file during tests.

## 0.14.0 (2016-07-08)

### New features

- Add [elixir](http://elixir-lang.org) project type.
- Add [emacs-cask](https://github.com/cask/cask) project type.
- Add [boot-clj](https://github.com/boot-clj/boot) project type.
- Add [racket](http://racket-lang.org) project type.
- Add support for projects using gradlew script.
- Prefer Haskell stack projects over cabal projects.
- Add ability to use elisp functions for test, compile and run commands.
- Consider `TAGS` and `GTAGS` root markers.
- Add relation between the `.h`, `.cxx`, `.ixx` and `.hxx` files in `projectile-other-file-alist`.
- Add relation between the `.hpp` and `.cc` files in `projectile-other-file-alist`.
- Add support to specify project name either via `.dir-locals.el` or by providing a customized `projectile-project-name-function`.
- Add a command to switch between open projects (`projectile-switch-open-project`).
- Add a command to edit the .dir-locals.el file of the project (`projectile-edit-dir-locals`).
- Add file local variable `projectile-project-root`, which allows overriding the project root on a per-file basis. This allows navigating a different project from, say, an org file in a another git repository.
- Add `projectile-grep-finished-hook`.
- Ignore file suffixes listed in `projectile-globally-ignored-file-suffixes` when using `projectile-grep` and `projectile-ag`.
- Add `projectile-replace-regexp`, which supports replacement by regexp within a project. `projectile-replace` is now used solely for literal replacements.
- New command `projectile-run-shell` (<kbd>C-c p x s</kbd>).
- New command `projectile-run-eshell` (<kbd>C-c p x e</kbd>).
- New command `projectile-run-term` (<kbd>C-c p x t</kbd>).
- [#971](https://github.com/bbatsov/projectile/pull/971): Let user unignore files in `.projectile` with the ! prefix.
- Add a command to add all projects in a directory to the cache (`projectile-discover-projects-in-directory`).
- Add a command to list dirty version controlled projects (`projectile-browse-dirty-projects`).

### Changes

- Prefer ag's internal .gitignore parsing.
- Added variable to control use of external find-tag implementations.
- Specify `--with-keep.source` argument when installing R projects

### Bugs fixed

- [#871](https://github.com/bbatsov/projectile/issues/871): Stop advice for `compilation-find-file` to override other advices.
- [#557](https://github.com/bbatsov/projectile/issues/557): stack overflow in `projectile-find-tag`.
- [#955](https://github.com/bbatsov/projectile/issues/955): Error while toggling between test and source file.
- [#952](https://github.com/bbatsov/projectile/issues/952): VCS submodules brought in even though not descendent of project root.
- [#576](https://github.com/bbatsov/projectile/issues/576): `projectile-replace` stomps regular expressions.
- [#957](https://github.com/bbatsov/projectile/pull/957): When opening a specified file from the terminal, do not error inside of `projectile-cache-current-file`.
- [#984](https://github.com/bbatsov/projectile/pull/984): Error when a project is a symlink that changes target.
- [#1013](https://github.com/bbatsov/projectile/issues/1013): `projectile-project-buffer-p` may return incorrect result on Windows.

## 0.13.0 (2015-10-21)

### New features

- Add `projectile-before-switch-project-hook`.
- Add the ability to specify the project type via `.dir-locals.el`.
- Add support for projects using Midje.
- Add the ability to create missing tests automatically (controlled via the `projectile-create-missing-test-files` defcustom).
- Add the ability to dynamically decide if a project should be added to `projectile-known-projects` (via new `projectile-ignored-project-function` defcustom).
- Add the ability to register new project types dynamically with `projectile-register-project-type`.
- Add the ability to specify a project compilation and test commands via `.dir-locals.el`.
This is done via the variables `projectile-project-compilation-cmd` and `projectile-project-test-cmd`.
- [#489](https://github.com/bbatsov/projectile/issues/489): New interactive command `projectile-run-project`.
- Optionally run [monky](http://ananthakumaran.in/monky/) on Mercurial projects.
- Add the ability to specify a project compilation directory relative to the root directory via `.dir-locals.el` with the variable `projectile-project-compilation-dir`.
- When there is a selected region, projectile-ag, projectile-grep, projectile-replace and projectile-find-tag uses its content as a search term instead of symbol at point.

### Changes

- Rename `projectile-switch-project-hook` to `projectile-after-switch-project-hook`.
- `projectile-compile-project` now offers appropriate completion
targets even when called from a subdirectory.
- Add an argument specifying the regexp to search to `projectile-grep`.
- Use `helm-projectile-grep` instead of `helm-find-file` when selecting a project.
- Omit current buffer from `projectile-switch-to-buffer` and `projectile-switch-to-buffer-other-window` choices.

### Bugs fixed

- [#721](https://github.com/bbatsov/projectile/issues/721#issuecomment-100830507): Remove current buffer from `helm-projectile-switch-project`.
- [#667](https://github.com/bbatsov/projectile/issues/667) Use `file-truename` when caching filenames to prevent duplicate/symlinked filepaths from appearing when opening a project file.
- [#625](https://github.com/bbatsov/projectile/issues/625): Ensure the directory has a trailing slash while searching for it.
- [#763](https://github.com/bbatsov/projectile/issues/763): Check for `projectile-use-git-grep` in `helm-projectile-grep`
- Fix `projectile-parse-dirconfig-file` to parse non-ASCII characters properly.

## 0.12.0 (2015-03-29)

### New features

- Replace Helm equivalent commands in `projectile-commander` when using Helm.
- Add replacement commands projectile-grep, projectile-ack and projectile-ag with their Helm versions.
- Add virtual directory manager that allows to create/update (add or delete files) a Dired buffer based on Projectile files.
- Add a new Helm command: `helm-projectile-find-file-in-known-projects` that opens all files in all known projects.
- Add an action for `helm-projectile-switch-project` to delete multiple marked projects.
- Add the ability to retrieve files in all sub-projects under a project root.
- Add `projectile-find-file-dwim` and `helm-projectile-find-file-dwim` commands.
- Provide actual Helm commands for common Projectile commands.
- Use existing Helm actions and map in `helm-find-files` that allows `helm-source-projectile-files-list`
to behave like `helm-find-files`, such as multifile selection and opening or delete on selected files.
- Add compile action to `helm-projectile`.
- Allows using Eshell and Magit outside of a project in `helm-projectile`.
- Add Helm action for incremental grep in the selected projects.
- Add command projectile-find-other-file  Switch between files with
the same  name but different extensions.
- Add Helm interface to switch project. For more details checkout the file
README.md.
- Make the mode line format customizable with `projectile-mode-line`
- Add support for `cargo.toml` projects
- Try to use projectile to find files in compilation buffers
- Support `helm` as a completion system
- New command `projectile-project-info` displays basic info about the current project.
- New `defcustom` `projectile-globally-ignored-buffers` allows you to ignore
buffers by name
- New `defcustom` `projectile-globally-ignored-file-suffixes` allows
you to globally ignore files with particular extensions

### Changes

- get-other-files returns more accurate results for files with the same name placed under different directories
- Collect search tool (`grep`, `ag`, `ack`) keybindings under a common keymap prefix (`C-c p s`)
- Remove `defcustom` `projectile-remember-window-configs` in favor of
`persp-projectile.el`.
- Progress reporter for the native indexing method.

### Bugs fixed

- Fix `projectile-regenerate-tags` to work in directories that include spaces.
- Prevent `projectile-kill-buffers` from trying to kill indirect
buffers.
- [#412](https://github.com/bbatsov/projectile/issues/412): Handle multiple possible targets in `projectile-toggle-between-implementation-or-test`.

## 0.11.0 (2014-05-27)

### New features

- Added support for default file glob pattern to `projectile-grep`
- added file existence cache with defcustoms
`projectile-file-exists-remote-cache-expire` and
`projectile-file-exists-local-cache-expire`.
- added new defcustoms `projectile-project-root-files-top-down-recurring`,
`projectile-project-root-files-bottom-up` and
`projectile-project-root-files-functions`.
- Added new command `projectile-save-project-buffers`.
- Added new command `projectile-cleanup-known-projects`.
- Added new commands `projectile-display-buffer`
and `projectile-find-dir-other-window`.
- Added new interactive function `projectile-project-buffers-other-buffer`
which runs new `projectile-project-buffers-non-visible` function, the former
is bound to `C-c p ESC`.
- New variable `projectile-enable-idle-timer` turns on an idle timer
which runs the hook `projectile-idle-timer-hook` every
`projectile-idle-timer-seconds` seconds when non-nil.
- New defcustom `projectile-remember-window-configs` will make
`projectile-switch-project` restore the most recent window configuration (if
any) of the target project.
- New command `projectile-run-command-in-root`.
- New command `projectile-run-shell-command-in-root`.
- New command `projectile-run-async-shell-command-in-root`.
- New defcustom `projectile-use-git-grep` will make `projectile-grep` use `git grep`
for git projects.
- Added new `projectile-commander` methods ?v and ?R which run
`projectile-vc` and `projectile-regenerate-tags`, respectively.
- `projectile-vc` will use `magit-status` if available.
- New functions `projectile-find-implementation-or-test` and
`projectile-find-implementation-or-test-other-window`, the latter is
bound to `C-c p 4 t`.
- New defcustoms `projectile-test-prefix-function` and `projectile-test-suffix-function`
allow users to customize how projectile identifies test files by project type.
- `projectile-grep` will ask for a file pattern if invoked with a
prefix argument.
- Subversion checkouts are now automatically detected.
- CVS checkouts are now automatically detected.
- added `projectile-persp-switch-project` command to make perspective
mode work along with projectile.
- Changed `projectile-mode-line-lighter` to a defcustom variable to make
mode line indicator prefix customizable.
- New command `projectile-find-file-in-known-projects`.
- New defcustom `projectile-ignored-projects` allows you to specify projects
that shouldn't be added to the known projects list.
- New command `projectile-remove-current-project-from-known-projects`.
- New defcustom `projectile-buffers-filter-function`.
- New defcustom `projectile-sort-order`.
- New function `projectile-process-current-project-buffers`.
- New function `projectile-process-current-project-files`.

### Changes

- The presence of a `Makefile` is no longer taken as an indicator
of the project root by default, since recursive make is unfortunately
a common occurrence (affects `projectile-project-root-files`).
- Projectile is now able to find the project pertaining to a symlink
pointing to a version-controlled file.
- Drop `projectile-ack-function` defcustom.
- `projectile-command-map` is now the keymap referenced by the
`projectile-keymap-prefix` in `projectile-mode-map`. This allows
modification of the inner map, and allows additional prefix keys to
reference it.

### Bugs fixed

- Modified `projectile-ack` to append to `ack-and-a-half-arguments`
instead of overriding them.
- [#229] Fix `projectile-find-file-in-directory`'s behavior for project directories
- `projectile-toggle-between-implementation-or-test` shows
understandable error if current buffer is not visiting a file.
- [#244] Correct folder picked up by `projectile-ack` after project-switch.
- [#182] Invalidate project cache if .projectile is modified.

## 0.10.0 (2013-12-09)

### New features

- Added new command `projectile-find-file-other-window`.
- Added new command `projectile-switch-to-buffer-other-window`.
- Added new command `projectile-find-file-in-directory` that allows
you to jump to files in any directory.
- `.projectile` is now always taken into account.
- `projectile-switch-project`'s behavior is now customizable via
`projectile-switch-project-action`.
- Added support for Gradle projects.
- Added support for `Ag`.
- Added new command `projectile-purge-file-from-cache`.
- Added new command `projectile-purge-dir-from-cache`.
- Added new command `projectile-find-tag`.
- Added new command `projectile-commander`. It allows you to quickly
run many Projectile commands with a single key. Very useful as a
project-switching action.
- `projectile-switch-project` now supports a prefix argument. When it's present
the switch action is `projectile-commander`.

### Changes

- Replaced variable `projectile-use-native-indexing` with `projectile-indexing-method`.
- Corrected grammar on error message for not being in a project.

### Bug fixes

- `projectile-find-test-file` now properly displays only test files (#145).

## 0.9.2 (2013-07-16)

### New features

- `projectile-invalidate-cache` now accepts a prefix argument. When
present you'll be prompted for the project whose cache to
invalidate.
- New command `projectile-find-dir` works similar to
`projectile-find-file` - displays the project's dirs and opens them
with `dired`. It's bound to `C-c p d`.
- Added support for `grizzl` as a completion system.
- Added support for `fossil` projects.
- Added support for `Symfony 2` project.
- New command `projectile-clear-known-projects` removes all known projects.
- New command `projectile-remove-known-project` prompts you for a known project to remove.

### Bugs fixed

- Fixed `projectile-replace`, which was broken from the use of relative paths
- #103 - `projectile-switch-project` does not require a project to work
- Don't show hidden buffers in projectile-project-buffers

### Changes

- Rebound `projectile-compile-project` to <kbd>C-c p c</kbd>
- Rebound `projectile-dired` to <kbd>C-c p D</kbd>
- Reworked `projectile-compile-project` and `projectile-test-project`
to be smarter, more configurable and closer in behavior to the stock
`compile` command
- `projectile-switch-project` (<kbd>C-c p s</kbd>) now runs `projectile-find-file` instead of `dired`.

## 0.9.1 (2013-04-26)

### New features

- Display recentf files in helm-projectile.

### Bugs fixed

- #95 - handle properly missing project root

## 0.9.0 (2013-04-24)

### New features

- Use fast external tools to find project files when possible. This is the default option on all Unices.
- Removed obsolete command `projectile-reindex-project`.
- Removed obsolete command `projectile-open`.
- Introduced support for finding tests and switching between code and tests.
- Implement basic project type detection.
- Add a simple version reporting command projectile-version.
- Display relative paths to project files instead of disambiguated filenames.
- Directories listed in .projectile file are excluded when tags are generated.
- Remembers visited projects and may switch between them with `projectile-switch-project`.
- Supports `lein {compile|test}` in Clojure projects.
- Support projects only for subdirs of the project root.
- Add the ability to manually cache files.

### Bugs fixed

- #57 - properly set the current working dir, before invoking shell commands
- #71 - correct regenerate tags keybinding in the README

### Misc

- Move menu entry under `Tools`
- Show indexing message only when doing native project indexing
- Massive performance improvements
