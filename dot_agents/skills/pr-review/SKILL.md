---
name: review-pr-comments
description: Review GitHub pull request feedback thread by thread, decide whether to accept, iterate, or reject each actionable item, then reply safely and consistently.
based_on: https://github.com/pamelafox/review-pr-comments/blob/main/.agents/skills/review-pr-comments/SKILL.md
---

We have received feedback on the current active pull request. Together, we will
review it thread by thread and decide whether to accept, iterate on, or reject
each actionable suggestion.

## Scope

This skill covers all pull request feedback that may require triage or a response, including:

- PR conversation comments on the pull request itself
- Review summary comments attached to a review
- Inline review comments on changed lines
- Replies within existing review threads
- Bot comments when they are actionable

Treat review **threads** as the primary unit of work, not individual comments in isolation.

## Definitions

Use these decision labels consistently:

- `accept`: implement the reviewer’s requested change substantially as proposed
- `iterate`: address the underlying concern, but with a different, narrower, or improved solution
- `reject`: make no code change and explain why the suggestion is incorrect, out of scope, already addressed, stale, or conflicts with project guidelines

Not every comment requires a code change. Questions, acknowledgements, and already-addressed feedback may only need a reply.

## Steps to follow

1. Identify the active pull request.
   - Confirm the repository, pull request number, and current head SHA.
   - If no active pull request can be identified, ask the user which pull request to review.
   - If the pull request changes during the session, re-fetch it before replying or resolving anything.

2. Fetch all relevant review data.
   - Prefer the active pull request tooling if it is available.
   - Otherwise use GitHub tooling or the GitHub CLI.
   - Fetch, with pagination if necessary:
     - PR conversation comments
     - review summaries
     - inline review comments
     - comment replies
     - review thread status, including whether a thread is resolved
     - outdated or stale thread/comment state when available
   - Do not assume a single endpoint returns the complete review model.

3. Normalize the results into actionable items.
   - Group related inline comments and replies into review threads.
   - Keep enough metadata for each item:
     - author
     - comment type
     - file path and line, if inline
     - thread status
     - whether the feedback is current or outdated
     - whether it appears already addressed on the current branch
     - whether it came from a human or a bot
   - Avoid discussing the same thread multiple times.

4. Present a summary list.
   - Show a concise numbered list of actionable threads/comments.
   - Include a one-sentence summary for each item.
   - Mark items that are outdated, already resolved, duplicated, or likely already addressed.

5. Review one thread at a time.
   - Present the full context of the current thread or PR comment.
   - Explain your recommendation: `accept`, `iterate`, or `reject`.
   - Base the recommendation on correctness, maintainability, project conventions, scope, and whether the feedback is still relevant on the current head.
   - Ask the user for a decision before making any code changes.

6. After the user decides:
   - If the decision is `accept` or `iterate`:
     - inspect the surrounding code carefully
     - make the smallest coherent change that addresses the concern
     - validate the change when practical with relevant tests, linting, or other checks
     - summarize what changed and any caveats
   - If the decision is `reject`:
     - do not change code
     - prepare a concise, respectful explanation

7. Before moving to the next thread:
   - wait for the user to confirm that the change summary is satisfactory
   - note whether the thread appears fully addressed, partially addressed, or intentionally rejected
   - if multiple comments overlap, mention that clearly and avoid redundant work

8. Before posting replies:
   - re-fetch the pull request or review thread state if there is any chance the branch changed
   - confirm the feedback is still current
   - avoid replying based on stale comment data

9. Reply on GitHub.
   - Reply once per actionable thread or PR comment where practical, not necessarily once per individual comment
   - Consolidate duplicate or closely related feedback when that makes the response clearer
   - For accepted or iterated changes, explain what was changed
   - For rejected changes, explain why no code change was made
   - Only mention a commit SHA if a real pushed commit exists

10. Resolve conversations only when appropriate.
   - Resolve a review thread only if the concern has actually been addressed and the team workflow allows authors to resolve threads
   - Do not auto-resolve rejected, ambiguous, or stale threads unless the user explicitly wants that
   - If team norms suggest reviewers should verify the fix first, leave the thread unresolved and state that in the reply

## How to reply to PR review comments

This guide explains how to reply directly to inline review comments on GitHub pull requests.

### API endpoint

To reply to an inline PR comment, use:

```http
POST /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies
```

With body:

```json
{
  "body": "Your reply message"
}
```

### Using gh CLI

```bash
gh api repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies \
  -X POST \
  -f body="Your reply message"
```

### Workflow

1. **Fetch review comments**: Retrieve review comments and related context, using pagination if necessary:

   ```bash
   gh api repos/{owner}/{repo}/pulls/{pull_number}/comments
   ```

2. **Identify the correct comment ID**:
   - Use the numeric `id` from the review comment object
   - Do **not** use `node_id`
   - Verify that you are replying to the correct parent review comment for the API or tool you are using

3. **Post a reply**:

   ```bash
   gh api repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies \
     -X POST \
     -f body="Addressed in the latest update"
   ```

### Example replies

For accepted changes:

- "Accepted — addressed in the latest update."
- "Good catch. I updated this to use {new_approach}."

For iterated changes:

- "I addressed the underlying issue, but used a slightly different approach here: {brief explanation}."
- "Implemented a narrower version of this to preserve {constraint}."

For rejected changes:

- "I am not changing this because {reason}."
- "I believe the current implementation is correct here because {reason}."

For already-addressed or stale feedback:

- "This appears to already be addressed on the current branch."
- "I did not make an additional change because this comment is outdated relative to the latest diff."

For questions:

- "Thanks — clarified this in the code and kept the implementation as is."
- "Good question. I reviewed this path and did not make a code change because {reason}."

## Notes

- The `comment_id` is the numeric ID from the comment object, not the `node_id`
- Replies appear as threaded responses under the original review comment
- Not every comment needs a separate reply; prefer one reply per actionable thread when appropriate
- Be cautious with bot comments: reply when they are actionable, not just noisy
- Do not claim something was fixed in a commit unless that commit actually exists on the pull request branch

### Resolving conversations

To resolve (mark as resolved) PR review threads, use the GraphQL API.

1. **Get thread IDs**: Query for review threads, using pagination if needed:

   ```bash
   gh api graphql -f query='
   query {
     repository(owner: "{owner}", name: "{repo}") {
       pullRequest(number: {pull_number}) {
         reviewThreads(first: 50) {
           nodes {
             id
             isResolved
             isOutdated
             comments(first: 10) {
               nodes { id body path }
             }
           }
         }
       }
     }
   }'
   ```

2. **Resolve a thread**:

   ```bash
   gh api graphql -f query='
   mutation {
     resolveReviewThread(input: {threadId: "PRRT_xxx"}) {
       thread { isResolved }
     }
   }'
   ```

3. **Resolve multiple threads at once**:

   ```bash
   gh api graphql -f query='
   mutation {
     t1: resolveReviewThread(input: {threadId: "PRRT_xxx"}) { thread { isResolved } }
     t2: resolveReviewThread(input: {threadId: "PRRT_yyy"}) { thread { isResolved } }
   }'
   ```

The thread ID starts with `PRRT_` and can be found in the GraphQL query response.

Only resolve threads when all of the following are true:

- the concern has been addressed or intentionally closed out
- the thread is still relevant to the current head
- resolving the thread matches the repository’s review norms
- the user wants the thread resolved now

Note: This skill can be removed once the GitHub MCP server has added built-in support for replying to PR review comments and resolving threads.
See:
https://github.com/github/github-mcp-server/issues/1323
https://github.com/github/github-mcp-server/issues/1768
