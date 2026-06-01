use emacs::{defun, Env, IntoLisp, Result, Value};
use gix::ObjectId;

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

/// Discover and open a repository from a path
#[defun(user_ptr)]
fn repo_discover(path: String) -> Result<gix::Repository> {
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
fn upstream_full_name(
    repo: &gix::Repository,
    branch: &str,
) -> Result<Option<gix::refs::FullName>> {
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

/// Equivalent to `git rev-parse --verify --abbrev-ref SPEC`: the short symbolic name SPEC
/// resolves to (e.g. branch name) or nil if SPEC is not an existing ref.
///
/// Handles the common `BRANCH@{upstream}` / `BRANCH@{u}` form by looking up the branch's
/// fetch-direction tracking ref. Other `@{...}` expressions (reflog, `@{push}`, etc.) are
/// not implemented; an error is signalled so the caller falls back to git.
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
        return Ok(None);
    };
    let name = match reference.target() {
        gix::refs::TargetRef::Symbolic(target) => target.shorten().to_string(),
        gix::refs::TargetRef::Object(_) => reference.name().shorten().to_string(),
    };
    Ok(Some(name))
}

/// Equivalent to `git rev-parse --verify --symbolic-full-name SPEC`: the full
/// ref name SPEC resolves to (one level of symbolic indirection followed, so
/// `HEAD` resolves to `refs/heads/main`). Returns nil for non-existent refs.
/// Handles `BRANCH@{upstream}` / `BRANCH@{u}`; other `@{...}` shapes signal an
/// error so the caller falls back to git.
#[defun]
fn revparse_symbolic_full_name(
    repo: &gix::Repository,
    spec: String,
) -> Result<Option<String>> {
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
        return Ok(None);
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
        ConfigScope::All => true,
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
