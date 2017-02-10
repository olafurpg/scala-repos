package gitbucket.core.api

import gitbucket.core.util.JGitUtil
import gitbucket.core.util.JGitUtil.CommitInfo
import gitbucket.core.util.RepositoryName

import org.eclipse.jgit.diff.DiffEntry
import org.eclipse.jgit.api.Git

import java.util.Date

/**
  * https://developer.github.com/v3/repos/commits/
  */
case class ApiCommit(id: String,
                     message: String,
                     timestamp: Date,
                     added: List[String],
                     removed: List[String],
                     modified: List[String],
                     author: ApiPersonIdent,
                     committer: ApiPersonIdent)(
    repositoryName: RepositoryName, urlIsHtmlUrl: Boolean)
    extends FieldSerializable {
  val url =
    if (urlIsHtmlUrl) {
      ApiPath(s"/${repositoryName.fullName}/commit/${id}")
    } else {
      ApiPath(s"/api/v3/${repositoryName.fullName}/commits/${id}")
    }
  val html_url =
    if (urlIsHtmlUrl) {
      None
    } else {
      Some(ApiPath(s"/${repositoryName.fullName}/commit/${id}"))
    }
}

object ApiCommit {
  def apply(git: Git,
            repositoryName: RepositoryName,
            commit: CommitInfo,
            urlIsHtmlUrl: Boolean = false): ApiCommit = {
    val diffs = JGitUtil.getDiffs(git, commit.id, false)
    ApiCommit(
        id = commit.id,
        message = commit.fullMessage,
        timestamp = commit.commitTime,
        added = diffs._1.collect {
          case x if x.changeType == DiffEntry.ChangeType.ADD => x.newPath
        },
        removed = diffs._1.collect {
          case x if x.changeType == DiffEntry.ChangeType.DELETE => x.oldPath
        },
        modified = diffs._1.collect {
          case x
              if x.changeType != DiffEntry.ChangeType.ADD &&
              x.changeType != DiffEntry.ChangeType.DELETE =>
            x.newPath
        },
        author = ApiPersonIdent.author(commit),
        committer = ApiPersonIdent.committer(commit),
    )(repositoryName, urlIsHtmlUrl)
  }
  def forPushPayload(git: Git,
                     repositoryName: RepositoryName,
                     commit: CommitInfo): ApiCommit =
    apply(git, repositoryName, commit, true)
}
