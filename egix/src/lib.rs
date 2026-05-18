use emacs::{defun, Env, IntoLisp, Result, Value};
use gix::ObjectId;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "egix-module", defun_prefix = "egix", separator = "-")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

fn resolve_ref<'a>(
    repo: &'a gix::Repository,
    name: &str,
) -> Option<gix::Reference<'a>> {
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

/// Equivalent to `git rev-parse --short SPEC`. Returns nil if SPEC cannot resolve.
#[defun]
fn revparse_short(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    reject_reflog_revspec("egix-revparse-short", spec.as_str())?;
    let Ok(id) = repo.rev_parse_single(spec.as_str()) else {
        return Ok(None);
    };
    Ok(Some(id.shorten_or_id().to_string()))
}

/// Equivalent to `git rev-parse --verify --abbrev-ref SPEC`: the short symbolic name SPEC
/// resolves to (e.g. branch name) or nil if SPEC is not an existing ref.
///
/// Handles the common `BRANCH@{upstream}` / `BRANCH@{u}` form by looking up the branch's
/// fetch-direction tracking ref. Other `@{...}` expressions (reflog, `@{push}`, etc.) are
/// not implemented; nil is returned so the caller can fall back to git.
#[defun]
fn revparse_abbrev_ref(repo: &gix::Repository, spec: String) -> Result<Option<String>> {
    if let Some(branch) = spec
        .strip_suffix("@{upstream}")
        .or_else(|| spec.strip_suffix("@{u}"))
    {
        let Some(reference) = resolve_ref(repo, branch) else {
            return Ok(None);
        };
        let Some(Ok(tracking)) =
            reference.remote_tracking_ref_name(gix::remote::Direction::Fetch)
        else {
            return Ok(None);
        };
        return Ok(Some(tracking.shorten().to_string()));
    }
    if spec.contains("@{") {
        // Reflog (@{N}), push (@{push}), previous-checkout (@{-N}) etc. are not
        // implemented here. Signal an error rather than returning Ok(None) so
        // callers can distinguish "unsupported syntax" (let the caller decide
        // what to do, typically fall back to git CLI) from a definitive
        // "no such ref" answer (Ok(None)).
        return Err(emacs::Error::msg(format!(
            "egix-revparse-abbrev-ref: unsupported revspec `{spec}`"
        )));
    }
    let Some(reference) = resolve_ref(repo, spec.as_str()) else {
        return Ok(None);
    };
    // git's --abbrev-ref follows one level of symbolic ref (e.g. HEAD -> main).
    let name = match reference.target() {
        gix::refs::TargetRef::Symbolic(target) => target.shorten().to_string(),
        gix::refs::TargetRef::Object(_) => reference.name().shorten().to_string(),
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
