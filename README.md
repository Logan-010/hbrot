# hbrot

Experimental Haskell rewrite of [mbrot](https://github.com/Logan-010/mbrot).

It's 2.6-3.0X slower, uses roughly twice as much memory, and the binary size is roughly 4.0-4.5x larger.
So don't use this, use mbrot. I just wrote this in learning Haskell. It's mostly pure aside
from image IO and multithreading. The actual derivative bailout algorithom is completely pure.

:p
