# warpless

A slim `warp` fork.

Last synchronized with `warp` December 20, 2023.
Latest commit porked over: `910117d448c3e775fc889e35b643c6f14ed4983d`.

## Changes

- No `https` support.
- No slowloris protection.
- No file descriptor cache.
- No file info cache.
- No response logger.
- No control over how much of an unread request body is read on a keepalive connection. It's all always read.
- No automatic Server response header.
- No control over preventing allocating too large of a response.
- No automatic setting of the `Alt-Svc` response header: you can just set it yourself in a response.
- `Expect: 100-continue` is eagerly replied to, rather than lazily upon trying to read the request body.
- Uncaught exceptions are ignored by default, rather than printed to stderr.
