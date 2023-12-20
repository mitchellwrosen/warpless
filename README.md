# warpless

A slim `warp` fork.

Up-to-date with `warp` commit `a2de346e41c0828560b850c1ea1ada849f990025`.

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
