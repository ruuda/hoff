# Done

 * Parse webhook data json -> event
 * Server to listen for webhooks
 * Run event loop
 * Run Git process and parse output
 * Add end-to-end test for retry after rejected push
 * Configurable Git credentials (just edit config of the daemon user)

# Up next

 * Test that the server continues serving after an invalid hook
 * Automated testing for all core functionality
 * Render webinterface pages
 * Serve webinterface pages
 * Test with GitHub

# Near-term

 * Support for multiple repositories
 * Support for commit message validation
 * Support for `--autosquash` rebasing
 * Generate ping event at startup
 * Client to generate API calls for ping
 * The ability to queue approved commits directly without requiring a pull
   request, for personal use
 * Support for basic review policy enforcement (whitelist reviewer usernames)
 * Append a "Reviewed-by" line to integrated commits
 * Set timezone to UTC+0 for bot (rebased) commits

# Long-term

 * A better way to keep track of proposed changes than pull requests,
   with support for dependent changes

# Eventually

 * A full code review system
