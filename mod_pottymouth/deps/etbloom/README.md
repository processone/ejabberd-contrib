Erlang Bloom Filter
=======

Based on Scalable Bloom Filters Paulo Sérgio Almeida, Carlos Baquero, Nuno Preguiça, David Hutchison
Information Processing Letters Volume 101, Issue 6, 31 March 2007, Pages 255-261

Provides scalable bloom filters that can grow indefinitely while
ensuring a desired maximum false positive probability. Also provides
standard partitioned bloom filters with a maximum capacity. Bit arrays
are dimensioned as a power of 2 to enable reusing hash values across
filters through bit operations. Double hashing is used (no need for
enhanced double hashing for partitioned bloom filters).

