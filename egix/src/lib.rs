use emacs::{defun, Env, IntoLisp, Result, Value};
use std::collections::{HashMap, HashSet};

emacs::plugin_is_GPL_compatible!();

/// Wrap a `Vec<T>` so it converts to an Emacs Lisp list `(t1 t2 ...)` when
/// `T: IntoLisp`. An empty vec becomes nil (since `()` is nil in elisp).
pub struct List<T>(pub Vec<T>);

impl<'e, T: IntoLisp<'e>> IntoLisp<'e> for List<T> {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        let vals: Vec<Value<'e>> = self
            .0
            .into_iter()
            .map(|item| item.into_lisp(env))
            .collect::<Result<_>>()?;
        env.list(&vals[..])
    }
}

/// Wrap a `Vec<(K, V)>` so it converts to an Emacs Lisp alist
/// `((k1 . v1) (k2 . v2) ...)` when `K: IntoLisp` and `V: IntoLisp`. An
/// empty vec becomes nil.
pub struct AList<K, V>(pub Vec<(K, V)>);

impl<'e, K: IntoLisp<'e>, V: IntoLisp<'e>> IntoLisp<'e> for AList<K, V> {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        let cells: Vec<Value<'e>> = self
            .0
            .into_iter()
            .map(|(k, v)| {
                let k = k.into_lisp(env)?;
                let v = v.into_lisp(env)?;
                env.cons(k, v)
            })
            .collect::<Result<_>>()?;
        env.list(&cells[..])
    }
}

#[emacs::module(name = "egix-module", defun_prefix = "egix", separator = "-")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

fn resolve_ref<'a>(repo: &'a gix::Repository, name: &str) -> Option<gix::Reference<'a>> {
    repo.find_reference(name).ok()
}

/// Removes `HOME` from the process environment for its lifetime, restoring the
/// previous value (if any) on drop.
///
/// Mutating the envirennement is ~probably~ safe in this case because
/// we are running on the main thread of emacs and there shouldn't be any worker
/// thread trying to read the environnement.
struct HomeGuard(Option<std::ffi::OsString>);

impl HomeGuard {
    fn suppress() -> Self {
        let prev = std::env::var_os("HOME");
        std::env::remove_var("HOME");
        HomeGuard(prev)
    }
}

impl Drop for HomeGuard {
    fn drop(&mut self) {
        if let Some(prev) = self.0.take() {
            std::env::set_var("HOME", prev);
        }
    }
}

/// Discover and open a repository from a path.
///
/// When SUPPRESS_HOME is non-nil, `HOME` is removed from the environment for
/// the duration of the open so gix resolves the global config the way a `git`
/// subprocess would. Callers pass this when their `HOME` is process-local and
/// not inherited by subprocesses (e.g. the value Emacs synthesizes on Windows),
/// so that gix and git agree on which config files to read.
#[defun(user_ptr, name = "-repo-discover-internal")]
fn repo_discover_internal(path: String, suppress_home: Value) -> Result<gix::Repository> {
    let _guard = suppress_home.is_not_nil().then(HomeGuard::suppress);
    let repo = gix::discover(&path)
        .map_err(|e| emacs::Error::msg(format!("Failed to discover repository: {}", e)))?;

    Ok(repo)
}

/// Get the current branch name from a Repository handle
/// Signals an error if the HEAD cannot be retrieved
/// Returns nil if HEAD is not pointing to a branch
#[defun]
fn repo_current_branch(repo: &gix::Repository) -> Result<Option<String>> {
    let head = repo.head()?;
    Ok(head
        .referent_name()
        .map(|reference| reference.shorten().to_string()))
}

/// Get repository root path from a Repository handle
#[defun]
fn repo_workdir(repo: &gix::Repository) -> Result<Option<String>> {
    let root = repo.workdir().map(|p| p.to_string_lossy().to_string());
    Ok(root)
}

/// Get repository git dir from a Repository handle
#[defun]
fn repo_gitdir(repo: &gix::Repository) -> Result<String> {
    Ok(repo.git_dir().to_string_lossy().to_string())
}

fn reject_reflog_revspec(fn_name: &str, spec: &str) -> Result<()> {
    if spec.contains("@{") {
        // gix and git can diverge on @{N}/@{-N}/@{push}/etc. — e.g. @{-1} returns a
        // stale OID when the previous branch is deleted. Defer to git.
        // TODO re-evaluate if https://github.com/GitoxideLabs/gitoxide/issues/2609 gets fixed
        return Err(emacs::Error::msg(format!(
            "{fn_name}: unsupported revspec `{spec}`"
        )));
    }
    Ok(())
}

/// Resolve SPEC to an object id (as a string). Returns nil if SPEC does not
/// resolve, so callers can short-circuit instead of consulting git.
#[defun]
fn revparse_single(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    reject_reflog_revspec("egix-revparse-single", spec.as_str())?;
    let Ok(id) = repo.rev_parse_single(spec.as_str()) else {
        return Ok(None);
    };
    Ok(Some(id.to_string()))
}

/// Equivalent to `git cat-file -t SPEC`: the type of the object SPEC resolves
/// to, as one of "commit", "tree", "blob", or "tag". Returns nil when SPEC does
/// not name an existing object.
#[defun]
fn object_type(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    reject_reflog_revspec("egix-object-type", spec.as_str())?;
    let Ok(id) = repo.rev_parse_single(spec.as_str()) else {
        return Ok(None);
    };
    let Ok(header) = repo.find_header(id.detach()) else {
        return Ok(None);
    };
    let kind = match header.kind() {
        gix::object::Kind::Commit => "commit",
        gix::object::Kind::Tree => "tree",
        gix::object::Kind::Blob => "blob",
        gix::object::Kind::Tag => "tag",
    };
    Ok(Some(kind.to_string()))
}

/// Equivalent to `git cat-file blob OID` / `git cat-file -p REV:PATH` for a blob:
/// the raw blob content. Errors (caller falls back to git) when SPEC is not a
/// blob, or the content is not valid UTF-8 or contains CR -- Emacs decodes git's
/// output with charset/EOL detection we do not replicate, so those go to git.
#[defun]
fn blob_content(repo: &gix::Repository, spec: String) -> Result<String> {
    reject_reflog_revspec("egix-blob-content", spec.as_str())?;
    let id = repo
        .rev_parse_single(spec.as_str())
        .map_err(|_| emacs::Error::msg("egix-blob-content: unresolved spec"))?;
    let object = repo
        .find_object(id.detach())
        .map_err(|_| emacs::Error::msg("egix-blob-content: object not found"))?;
    if object.kind != gix::object::Kind::Blob {
        return Err(emacs::Error::msg("egix-blob-content: not a blob"));
    }
    if object.data.contains(&b'\r') {
        return Err(emacs::Error::msg("egix-blob-content: CR in content"));
    }
    std::str::from_utf8(&object.data)
        .map(str::to_string)
        .map_err(|_| emacs::Error::msg("egix-blob-content: non-UTF8 content"))
}

/// Equivalent to `git ls-tree --full-tree REV -- FILE`.
/// Returns the entry line `<mode> <type> <oid>\t<path>`, or "" if FILE is absent.
/// Signals when REV is not a tree, so the caller falls back to git.
#[defun]
fn ls_tree_entry(repo: &gix::Repository, rev: String, file: String) -> Result<String> {
    reject_reflog_revspec("egix-ls-tree-entry", rev.as_str())?;
    let id = repo
        .rev_parse_single(rev.as_str())
        .map_err(|_| emacs::Error::msg("egix-ls-tree-entry: unresolved rev"))?;
    let tree = repo
        .find_object(id.detach())
        .map_err(|_| emacs::Error::msg("egix-ls-tree-entry: object not found"))?
        .peel_to_tree()
        .map_err(|_| emacs::Error::msg("egix-ls-tree-entry: not a tree-ish"))?;
    let Some(entry) = tree
        .lookup_entry_by_path(file.as_str())
        .map_err(|e| emacs::Error::msg(e.to_string()))?
    else {
        return Ok(String::new());
    };
    let mode = entry.mode();
    let kind = if mode.is_tree() {
        "tree"
    } else if mode.is_commit() {
        "commit"
    } else {
        "blob"
    };
    Ok(format!(
        "{:06o} {} {}\t{}\n",
        mode.value(),
        kind,
        entry.oid(),
        file
    ))
}

/// Whether the index differs from HEAD for FILE (git `diff --quiet --cached`;
/// true = differs = git exit 1). FILE is repo-relative. Compares the index entry
/// to HEAD's tree entry by oid and mode; an intent-to-add entry counts as absent
/// and an unborn HEAD as an empty tree. Signals (caller falls back to git) for a
/// submodule path, an unmerged entry, or a missing index.
#[defun]
fn index_differs_from_head(repo: &gix::Repository, file: String) -> Result<bool> {
    let index = repo
        .index()
        .map_err(|_| emacs::Error::msg("egix-index-differs-from-head: no index"))?;
    let staged = match index.entry_by_path(gix::bstr::BStr::new(file.as_bytes())) {
        None => None,
        Some(entry) => {
            if entry.stage_raw() != 0 {
                return Err(emacs::Error::msg("egix-index-differs-from-head: unmerged entry"));
            }
            if entry.mode.is_submodule() {
                return Err(emacs::Error::msg("egix-index-differs-from-head: submodule"));
            }
            if entry.flags.contains(gix::index::entry::Flags::INTENT_TO_ADD) {
                None
            } else {
                let mode = entry.mode.to_tree_entry_mode().ok_or_else(|| {
                    emacs::Error::msg("egix-index-differs-from-head: unsupported mode")
                })?;
                Some((entry.id, mode.value()))
            }
        }
    };
    let head = match repo.head_tree() {
        Err(_) => None, // unborn HEAD: treat as an empty tree
        Ok(tree) => match tree
            .lookup_entry_by_path(file.as_str())
            .map_err(|e| emacs::Error::msg(e.to_string()))?
        {
            None => None,
            Some(entry) => {
                let mode = entry.mode();
                if mode.is_commit() {
                    return Err(emacs::Error::msg("egix-index-differs-from-head: submodule"));
                }
                Some((entry.oid().to_owned(), mode.value()))
            }
        },
    };
    // Staged iff the index and HEAD entries are not identical (oid + mode).
    Ok(staged != head)
}

fn hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

/// Expand a `git log --format=` string against COMMIT, without the trailing
/// newline git writes per entry. Unsupported placeholders (dates, mailmap
/// `%aN`, `%D`, ...) and non-UTF-8 output are rejected so the caller falls back
/// to git rather than emit a wrong answer.
fn expand_commit_format(
    commit: &gix::Commit<'_>,
    format: &str,
    decorations: Option<&Decorations>,
    mailmap: Option<&gix::mailmap::Snapshot>,
) -> Result<String> {
    let unsupported = || emacs::Error::msg("egix-commit-format: unsupported format placeholder");

    let mut output: Vec<u8> = Vec::with_capacity(format.len());
    let mut remaining = format.bytes();
    while let Some(byte) = remaining.next() {
        if byte != b'%' {
            output.push(byte);
            continue;
        }
        match remaining.next().ok_or_else(unsupported)? {
            b'%' => output.push(b'%'),
            b'n' => output.push(b'\n'),
            b'x' => {
                let high = remaining
                    .next()
                    .and_then(hex_value)
                    .ok_or_else(unsupported)?;
                let low = remaining
                    .next()
                    .and_then(hex_value)
                    .ok_or_else(unsupported)?;
                output.push((high << 4) | low);
            }
            b'H' => output.extend_from_slice(commit.id.to_string().as_bytes()),
            b'h' => output.extend_from_slice(commit.short_id()?.to_string().as_bytes()),
            b'D' => {
                let decorations = decorations.ok_or_else(unsupported)?;
                output.extend_from_slice(decorations.format(commit.id).as_bytes());
            }
            b's' => {
                let summary = commit.message()?.summary();
                output.extend_from_slice(&summary);
            }
            b'B' => output.extend_from_slice(commit.message_raw()?),
            actor @ (b'a' | b'c') => {
                let signature = if actor == b'a' {
                    commit.author()?
                } else {
                    commit.committer()?
                };
                match remaining.next().ok_or_else(unsupported)? {
                    b'n' => output.extend_from_slice(signature.name),
                    b'e' => output.extend_from_slice(signature.email),
                    field @ (b'N' | b'E') => {
                        let mailmap = mailmap.ok_or_else(unsupported)?;
                        let resolved = mailmap.resolve(signature);
                        let value = if field == b'N' {
                            &resolved.name
                        } else {
                            &resolved.email
                        };
                        output.extend_from_slice(value);
                    }
                    b't' => output.extend_from_slice(signature.seconds().to_string().as_bytes()),
                    _ => return Err(unsupported()),
                }
            }
            _ => return Err(unsupported()),
        }
    }
    String::from_utf8(output).map_err(|_| emacs::Error::msg("egix-commit-format: non-UTF8 output"))
}

/// Equivalent to `git log --no-walk --format=FORMAT SPEC --` for one commit, sans
/// git's trailing entry newline (the caller appends it). Nil when SPEC is not a
/// commit; errors on an unsupported placeholder so the caller falls back to git.
#[defun]
fn commit_format(repo: &gix::Repository, spec: String, format: String) -> Result<Option<String>> {
    reject_reflog_revspec("egix-commit-format", spec.as_str())?;
    let Ok(id) = repo.rev_parse_single(spec.as_str()) else {
        return Ok(None);
    };
    let Ok(commit) = repo.find_commit(id.detach()) else {
        return Ok(None);
    };
    Ok(Some(expand_commit_format(&commit, format.as_str(), None, None)?))
}

fn format_needs_mailmap(format: &str) -> bool {
    format.contains("%aN")
        || format.contains("%aE")
        || format.contains("%cN")
        || format.contains("%cE")
}

/// Equivalent to `git log --format=FORMAT [-n LIMIT] [REV]`, with REV defaulting
/// to HEAD, walking in git's default commit-time order. Each entry is followed
/// by a newline. Returns nil when REV does not resolve; errors (caller falls
/// back to git) on a range REV or an unsupported format placeholder.
#[defun]
fn log(
    repo: &gix::Repository,
    rev: Option<String>,
    limit: Option<usize>,
    format: String,
) -> Result<Option<String>> {
    let spec = rev.as_deref().unwrap_or("HEAD");
    reject_reflog_revspec("egix-log", spec)?;
    if spec.contains("..") {
        return Err(emacs::Error::msg("egix-log: range revspec unsupported"));
    }
    let Ok(tip) = repo.rev_parse_single(spec) else {
        return Ok(None);
    };
    let decorations = if format.contains("%D") {
        Some(Decorations::build(repo)?)
    } else {
        None
    };
    let mailmap = format_needs_mailmap(&format).then(|| repo.open_mailmap());

    use gix::revision::walk::Sorting;
    use gix::traverse::commit::simple::CommitTimeOrder;
    let walk = repo
        .rev_walk([tip.detach()])
        .sorting(Sorting::ByCommitTime(CommitTimeOrder::NewestFirst))
        .all()?;

    let mut output = String::new();
    for (n, info) in walk.enumerate() {
        if limit.is_some_and(|limit| n >= limit) {
            break;
        }
        let info = info.map_err(|e| emacs::Error::msg(e.to_string()))?;
        let commit = repo
            .find_commit(info.id)
            .map_err(|e| emacs::Error::msg(e.to_string()))?;
        output.push_str(&expand_commit_format(
            &commit,
            &format,
            decorations.as_ref(),
            mailmap.as_ref(),
        )?);
        output.push('\n');
    }
    Ok(Some(output))
}

/// Equivalent to `git rev-parse --short[=LENGTH] SPEC`. LENGTH is the minimum
/// abbreviation width; nil uses git's configured default. Returns nil if SPEC
/// cannot resolve.
#[defun]
fn revparse_short(
    repo: &gix::Repository,
    spec: String,
    length: Option<usize>,
) -> Result<Option<String>> {
    reject_reflog_revspec("egix-revparse-short", spec.as_str())?;
    let Ok(id) = repo.rev_parse_single(spec.as_str()) else {
        return Ok(None);
    };
    match length {
        None => Ok(Some(id.shorten_or_id().to_string())),
        Some(n) => {
            use gix::odb::store::prefix::disambiguate::Candidate;
            let n = n.min(repo.object_hash().len_in_hex());
            let candidate = Candidate::new(id.detach(), n)?;
            Ok(repo
                .objects
                .disambiguate_prefix(candidate)?
                .map(|p| p.to_string()))
        }
    }
}

/// Look up the upstream (`BRANCH@{upstream}` / `@{u}`) tracking ref of BRANCH
/// and return its full name, or `None` when BRANCH does not exist or has no
/// upstream. `remote_tracking_ref_name` does not cover the case where the
/// upstream itself is a local branch (`branch.<name>.remote = .`); fall back
/// to `remote_ref_name` then.
fn upstream_full_name(repo: &gix::Repository, branch: &str) -> Result<Option<gix::refs::FullName>> {
    let Some(reference) = resolve_ref(repo, branch) else {
        return Ok(None);
    };
    let is_local = reference
        .remote_name(gix::remote::Direction::Fetch)
        .map(|n| n.as_bstr() == ".")
        .unwrap_or(false);
    let upstream_ref = if is_local {
        reference
            .remote_ref_name(gix::remote::Direction::Fetch)
            .transpose()?
    } else {
        reference
            .remote_tracking_ref_name(gix::remote::Direction::Fetch)
            .transpose()?
    };
    Ok(upstream_ref.map(|r| r.into_owned()))
}

/// Shared tail for `--abbrev-ref` / `--symbolic-full-name` when SPEC is not a ref. Returns
/// `Some("")` when SPEC names a valid object (a raw commit hash, `HEAD~1`, ...) — git prints
/// nothing and exits 0 in that case — and `None` when SPEC resolves to nothing at all.
fn object_not_ref(repo: &gix::Repository, spec: &str) -> Option<String> {
    repo.rev_parse_single(spec).ok().map(|_| String::new())
}

/// Equivalent to `git rev-parse --verify --abbrev-ref SPEC`: the short symbolic name SPEC
/// resolves to (e.g. branch name). Returns an empty string when SPEC names a valid object
/// that is not a ref, or nil when SPEC resolves to nothing.
///
/// Handles the common `BRANCH@{upstream}` / `BRANCH@{u}` form by looking up the branch's
/// fetch-direction tracking ref. Other `@{...}` expressions (reflog, `@{push}`, etc.) are
/// not implemented; an error is signalled instead.
#[defun]
fn revparse_abbrev_ref(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    if let Some(branch) = spec
        .strip_suffix("@{upstream}")
        .or_else(|| spec.strip_suffix("@{u}"))
    {
        return Ok(upstream_full_name(repo, branch)?.map(|r| r.shorten().to_string()));
    }
    if spec.contains("@{") {
        return Err(emacs::Error::msg(format!(
            "egix-revparse-abbrev-ref: unsupported revspec `{spec}`"
        )));
    }
    let Some(reference) = resolve_ref(repo, spec.as_str()) else {
        return Ok(object_not_ref(repo, spec.as_str()));
    };
    let name = match reference.target() {
        gix::refs::TargetRef::Symbolic(target) => target.shorten().to_string(),
        gix::refs::TargetRef::Object(_) => reference.name().shorten().to_string(),
    };
    Ok(Some(name))
}

/// Equivalent to `git rev-parse --verify --symbolic-full-name SPEC`: the full
/// ref name SPEC resolves to (one level of symbolic indirection followed, so
/// `HEAD` resolves to `refs/heads/main`). Returns an empty string when SPEC
/// names a valid object that is not a ref, or nil when SPEC resolves to nothing.
/// Handles `BRANCH@{upstream}` / `BRANCH@{u}`; other `@{...}` shapes signal an error.
#[defun]
fn revparse_symbolic_full_name(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    if let Some(branch) = spec
        .strip_suffix("@{upstream}")
        .or_else(|| spec.strip_suffix("@{u}"))
    {
        return Ok(upstream_full_name(repo, branch)?.map(|r| r.as_bstr().to_string()));
    }
    if spec.contains("@{") {
        return Err(emacs::Error::msg(format!(
            "egix-revparse-symbolic-full-name: unsupported revspec `{spec}`"
        )));
    }
    let Some(reference) = resolve_ref(repo, spec.as_str()) else {
        return Ok(object_not_ref(repo, spec.as_str()));
    };
    let name = match reference.target() {
        gix::refs::TargetRef::Symbolic(target) => target.as_bstr().to_string(),
        gix::refs::TargetRef::Object(_) => reference.name().as_bstr().to_string(),
    };
    Ok(Some(name))
}

/// Equivalent to `git symbolic-ref REF_NAME`: the full target of a symbolic ref.
/// Returns nil if REF_NAME does not exist or is not symbolic.
#[defun]
fn symbolic_ref(repo: &gix::Repository, ref_name: String) -> Result<Option<String>> {
    Ok(symbolic_target(repo, ref_name.as_str(), false))
}

/// Equivalent to `git symbolic-ref --short REF_NAME`: shortened target of a symbolic ref.
#[defun]
fn symbolic_ref_short(repo: &gix::Repository, ref_name: String) -> Result<Option<String>> {
    Ok(symbolic_target(repo, ref_name.as_str(), true))
}

fn symbolic_target(repo: &gix::Repository, ref_name: &str, short: bool) -> Option<String> {
    let reference = resolve_ref(repo, ref_name)?;
    match reference.target() {
        gix::refs::TargetRef::Symbolic(target) => Some(if short {
            target.shorten().to_string()
        } else {
            target.as_bstr().to_string()
        }),
        gix::refs::TargetRef::Object(_) => None,
    }
}

/// Equivalent to `git remote`: the configured remote names in git's sorted,
/// de-duplicated order. Empty list (nil) when the repository has no remotes.
#[defun]
fn remote_names(repo: &gix::Repository) -> Result<List<String>> {
    Ok(List(
        repo.remote_names()
            .into_iter()
            .map(|name| name.to_string())
            .collect(),
    ))
}

/// Equivalent to `git remote get-url NAME`: the fetch URL of remote NAME with
/// `url.<base>.insteadOf` rewrites applied. Returns nil when NAME is not a
/// configured remote or has no fetch URL.
#[defun]
fn remote_get_url(repo: &gix::Repository, name: String) -> Result<Option<String>> {
    let remote = match repo.try_find_remote(name.as_str()) {
        None => return Ok(None),
        Some(remote) => remote?,
    };
    Ok(remote
        .url(gix::remote::Direction::Fetch)
        .map(|url| url.to_bstring().to_string()))
}

/// list of prefix and suffix resolve refs
const REF_REV_PARSE_RULES: &[(&str, &str)] = &[
    ("", ""),
    ("refs/", ""),
    ("refs/tags/", ""),
    ("refs/heads/", ""),
    ("refs/remotes/", ""),
    ("refs/remotes/", "/HEAD"),
];

/// Compute the shortest refname that is unambigous (given the list of ref in the repo)
fn short_refname(full: &str, existing: &HashSet<String>) -> String {
    for candidate in (1..REF_REV_PARSE_RULES.len()).rev() {
        let (prefix, suffix) = REF_REV_PARSE_RULES[candidate];
        let Some(tail) = full
            .strip_prefix(prefix)
            .and_then(|s| s.strip_suffix(suffix))
        else {
            continue;
        };
        if tail.is_empty() {
            continue;
        }
        let ambiguous = REF_REV_PARSE_RULES
            .iter()
            .enumerate()
            .any(|(rule, &(p, s))| {
                rule != candidate && existing.contains(&format!("{p}{tail}{s}"))
            });
        if !ambiguous {
            return tail.to_string();
        }
    }
    full.to_string()
}

/// Equivalent to `git for-each-ref NAMESPACE`
/// NAMESPACE is a ref directory ("refs/heads")
/// Returns a list of (SYMBOLIC_REF FULLNAME SHORTNAME) for each ref
#[defun]
fn for_each_ref(repo: &gix::Repository, namespace: String) -> Result<List<List<Option<String>>>> {
    let prefix = if namespace.ends_with('/') {
        namespace
    } else {
        format!("{namespace}/")
    };

    let platform = repo.references()?;

    // Collect every refs so the shortname can be computed
    let existing: HashSet<String> = platform
        .all()?
        .map(|reference| {
            reference
                .map(|r| r.name().as_bstr().to_string())
                .map_err(|e| emacs::Error::msg(e.to_string()))
        })
        .collect::<Result<_>>()?;

    let mut references = platform
        .prefixed(prefix.as_str())?
        .map(|reference| reference.map_err(|e| emacs::Error::msg(e.to_string())))
        .collect::<Result<Vec<_>>>()?;
    references.sort_by(|a, b| a.name().as_bstr().cmp(b.name().as_bstr()));

    let entries = references
        .into_iter()
        .map(|reference| {
            let symref = match reference.target() {
                gix::refs::TargetRef::Symbolic(target) => Some(target.as_bstr().to_string()),
                gix::refs::TargetRef::Object(_) => None,
            };
            let full = reference.name().as_bstr().to_string();
            let short = short_refname(&full, &existing);
            List(vec![symref, Some(full), Some(short)])
        })
        .collect();

    Ok(List(entries))
}

/// One ref decorating a commit: its full name and whether it is a tag.
struct DecorationRef {
    name: String,
    is_tag: bool,
}

/// The `%D` decorations of a repository: the refs at each commit, and where
/// HEAD points.
struct Decorations {
    /// oid -> refs decorating it, each list ordered as git emits `%D`.
    refs: HashMap<gix::ObjectId, Vec<DecorationRef>>,
    /// The commit HEAD resolves to, or None when unborn.
    head_oid: Option<gix::ObjectId>,
    /// Full name of the branch HEAD points to, or None when detached.
    head_branch: Option<String>,
}

impl Decorations {
    fn build(repo: &gix::Repository) -> Result<Self> {
        // log.excludeDecoration switches the decorated set to a glob-filtered
        // all-refs set; unsupported, so defer to git.
        if repo
            .config_snapshot()
            .string("log.excludeDecoration")
            .is_some()
        {
            return Err(emacs::Error::msg(
                "egix-decorations: log.excludeDecoration is set",
            ));
        }
        let refs = Self::decoratable_refs(repo)?;
        let head = repo.head()?;
        let head_branch = head.referent_name().map(|name| name.as_bstr().to_string());
        let head_oid = head.id().map(|id| id.detach());
        Ok(Self {
            refs,
            head_oid,
            head_branch,
        })
    }

    /// git's `%D` with `--decorate=full` for OID: HEAD first (`HEAD -> <branch>`
    /// when on a branch, else a bare `HEAD`), then the refs at OID with `tag: `
    /// on tags. Empty string when nothing decorates OID.
    fn format(&self, oid: gix::ObjectId) -> String {
        let mut tokens: Vec<String> = Vec::new();
        let head_branch = match (self.head_oid == Some(oid)).then_some(&self.head_branch) {
            Some(Some(branch)) => {
                tokens.push(format!("HEAD -> {branch}"));
                Some(branch.as_str())
            }
            Some(None) => {
                tokens.push("HEAD".to_string());
                None
            }
            None => None,
        };
        if let Some(refs) = self.refs.get(&oid) {
            for reference in refs {
                if Some(reference.name.as_str()) == head_branch {
                    continue; // already shown as `HEAD -> <branch>`
                }
                if reference.is_tag {
                    tokens.push(format!("tag: {}", reference.name));
                } else {
                    tokens.push(reference.name.clone());
                }
            }
        }
        tokens.join(", ")
    }

    /// Collect the decoratable refs, peeled to their commit, as an oid -> refs
    /// map. Each oid's list is in git's `%D` order: the reverse of an ascending
    /// full-name sort.
    fn decoratable_refs(
        repo: &gix::Repository,
    ) -> Result<HashMap<gix::ObjectId, Vec<DecorationRef>>> {
        let references = repo.references()?;
        let mut refs: Vec<(String, gix::ObjectId, bool)> = Vec::new();
        for reference in references.all()?.peeled()? {
            let reference = reference.map_err(|e| emacs::Error::msg(e.to_string()))?;
            let name = reference.name().as_bstr().to_string();
            if !Self::is_decoratable(&name) {
                continue;
            }
            let is_tag = name.starts_with("refs/tags/");
            refs.push((name, reference.id().detach(), is_tag));
        }
        refs.sort_by(|a, b| a.0.cmp(&b.0));
        let mut map: HashMap<gix::ObjectId, Vec<DecorationRef>> = HashMap::new();
        for (name, oid, is_tag) in refs.into_iter().rev() {
            map.entry(oid)
                .or_default()
                .push(DecorationRef { name, is_tag });
        }
        Ok(map)
    }

    /// The ref namespaces git decorates by default: branches, remotes, tags,
    /// and the stash. Notes, bisect, replace, prefetch, etc. are excluded.
    fn is_decoratable(name: &str) -> bool {
        name.starts_with("refs/heads/")
            || name.starts_with("refs/remotes/")
            || name.starts_with("refs/tags/")
            || name == "refs/stash"
    }
}

#[derive(Copy, Clone)]
enum ConfigScope {
    All,
    Local,
    Global,
    System,
}

fn parse_config_scope(scope: Option<String>) -> Result<ConfigScope> {
    match scope.as_deref() {
        None | Some("all") => Ok(ConfigScope::All),
        Some("local") => Ok(ConfigScope::Local),
        Some("global") => Ok(ConfigScope::Global),
        Some("system") => Ok(ConfigScope::System),
        Some(other) => Err(emacs::Error::msg(format!(
            "egix-config: invalid scope '{other}', expected one of: local, global, system, all"
        ))),
    }
}

fn config_source_matches(scope: ConfigScope, source: gix::config::Source) -> bool {
    use gix::config::Source as S;
    match scope {
        // git config --list shows the real config sources; gix additionally
        // holds its own defaults (Api, e.g. gitoxide.credentials.terminalPrompt)
        // and env-derived overrides like HTTP_PROXY (EnvOverride) that git omits.
        ConfigScope::All => !matches!(source, S::Api | S::EnvOverride),
        // `git config --local` reads the repository config (.git/config) and
        // any per-worktree config in newer git layouts.
        ConfigScope::Local => matches!(source, S::Local | S::Worktree),
        // `git config --global` reads ~/.gitconfig and $XDG_CONFIG_HOME/git/config.
        ConfigScope::Global => matches!(source, S::User),
        // `git config --system` reads /etc/gitconfig (plus the install-time config).
        ConfigScope::System => matches!(source, S::System | S::GitInstallation),
    }
}

fn split_key(key: &str) -> Option<(&str, Option<&[u8]>, &str)> {
    let first_dot = key.find('.')?;
    let last_dot = key.rfind('.')?;
    let section = &key[..first_dot];
    let name = &key[last_dot + 1..];
    if section.is_empty() || name.is_empty() {
        return None;
    }
    let subsection = if first_dot == last_dot {
        None
    } else {
        Some(key[first_dot + 1..last_dot].as_bytes())
    };
    Some((section, subsection, name))
}

/// Equivalent to `git config [--SCOPE] --get-all KEY`. Returns the list of
/// values for KEY (in declared order, multi-value supported), or nil when
/// the key has no values in the requested scope.
#[defun]
fn config_get_all(
    repo: &gix::Repository,
    key: String,
    scope: Option<String>,
) -> Result<List<String>> {
    let scope = parse_config_scope(scope)?;
    let (section, subsection, name) = split_key(&key)
        .ok_or_else(|| emacs::Error::msg(format!("egix-config-get-all: invalid key '{key}'")))?;
    let snapshot = repo.config_snapshot();
    let mut filter =
        |meta: &gix::config::file::Metadata| -> bool { config_source_matches(scope, meta.source) };
    let values = snapshot
        .strings_filter_by(
            section,
            subsection.map(gix::bstr::BStr::new),
            name,
            &mut filter,
        )
        .map(|v| v.into_iter().map(|s| s.to_string()).collect())
        .unwrap_or_default();
    Ok(List(values))
}

/// Equivalent to `git config [--SCOPE] KEY`. Returns the effective single
/// value (last-wins per git semantics), or nil when the key has no values.
#[defun]
fn config_get(
    repo: &gix::Repository,
    key: String,
    scope: Option<String>,
) -> Result<Option<String>> {
    let scope = parse_config_scope(scope)?;
    let (section, subsection, name) = split_key(&key)
        .ok_or_else(|| emacs::Error::msg(format!("egix-config-get: invalid key '{key}'")))?;
    let snapshot = repo.config_snapshot();
    let mut filter =
        |meta: &gix::config::file::Metadata| -> bool { config_source_matches(scope, meta.source) };
    Ok(snapshot
        .string_filter_by(
            section,
            subsection.map(gix::bstr::BStr::new),
            name,
            &mut filter,
        )
        .map(|s| s.to_string()))
}

/// Equivalent to `git config [--SCOPE] --list`. Returns an alist
/// ((KEY1 . VALUE1) (KEY2 . VALUE2) ...) in declared order across the
/// requested scope. nil when there are no entries.
#[defun]
fn config_list(repo: &gix::Repository, scope: Option<String>) -> Result<AList<String, String>> {
    let scope = parse_config_scope(scope)?;
    let snapshot = repo.config_snapshot();
    let mut out: Vec<(String, String)> = Vec::new();
    for section in snapshot.sections() {
        if !config_source_matches(scope, section.meta().source) {
            continue;
        }
        let header = section.header();
        // Match `git config --list`: section + variable names are case-insensitive
        // and emitted lowercase; subsection names are case-sensitive and kept verbatim.
        let section_name = header.name().to_string().to_ascii_lowercase();
        let subsection = header.subsection_name().map(|s| s.to_string());
        for (key_name, value) in section.body().clone().into_iter() {
            let key_lower = key_name.as_ref().to_ascii_lowercase();
            let dotted = match &subsection {
                Some(sub) => format!("{}.{}.{}", section_name, sub, key_lower),
                None => format!("{}.{}", section_name, key_lower),
            };
            out.push((dotted, value.to_string()));
        }
    }
    Ok(AList(out))
}
