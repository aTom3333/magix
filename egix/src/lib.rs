use emacs::{defun, Env, IntoLisp, Result, Value};
use gix::ObjectId;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "egix-module", defun_prefix = "egix", separator = "-")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
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
    let root = repo.work_dir().map(|p| p.to_string_lossy().to_string());
    Ok(root)
}

/// Get repository git dir from a Repository handle
#[defun]
fn repo_gitdir(repo: &gix::Repository) -> Result<String> {
    Ok(repo.git_dir().to_string_lossy().to_string())
}

/// Get an object id (as a string) from a string that represents a single object
#[defun]
fn revparse_single(repo: &gix::Repository, spec: String) -> Result<String> {
    let id = repo.rev_parse_single(spec.as_str())?;
    Ok(id.to_string())
}
