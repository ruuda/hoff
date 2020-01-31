# Changelog

## 0.12.0

 * **Compatibility**: The schema of the state files has changed. Hoff v0.12.0
   can read v0.11.0 state files, so an upgrade is seamless, but a downgrade
   would require manual intervention.
 * Fix bug where Hoff would stop to try merging, after pushing to master fails
   because something else was pushed to master meanwhile.

## 0.11.0

 * Accept merge command anywhere in comments.
 * Take dependencies from Stackage LTS 14.21, up from 9.0.

## 0.10.0

 * Do not delete branches after merging a pull request, GitHub has that
   functionality now, and it can interfere with dependent pull requests.
 * Add overview pages that show everything in all repositories of an owner
   on one page.
 * Fix a bug in the queue position comment that the bot leaves.

## Older versions

 * TODO: Backfill the changelog.
