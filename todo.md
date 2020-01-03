# Up next

 * Automated testing for all core functionality

# Near-term

 * Add tests for multiproject code
 * Graceful shutdown when receiving sigterm
 * Add the ability to force a rebuild
 * Keep track of recently integrated pull requests, even after they were closed
 * Support for commit message validation
 * Generate ping event at startup
 * Client to generate API calls for ping
 * The ability to queue approved commits directly without requiring a pull
   request, for personal use
 * Append a "Reviewed-by" line to integrated commits
 * Set timezone to UTC+0 for bot (rebased) commits
 * Add the ability to filter pull request status by context:
   accepting pull request build status as valid is wrong,
   because GitHub makes Travis build a merge commit
 * Clean up multiproject code

# Done

 * Parse webhook data json -> event
 * Server to listen for webhooks
 * Run event loop
 * Run Git process and parse output
 * Add end-to-end test for retry after rejected push
 * Configurable Git credentials (just edit config of the daemon user)
 * Test that the server continues serving after an invalid hook
 * Render webinterface pages
 * Serve webinterface pages
 * Test with GitHub
 * Support for basic review policy enforcement (whitelist reviewer usernames)
 * Support for multiple repositories
 * Make API calls to leave comments and close pull requests
 * Support for `--autosquash` rebasing
