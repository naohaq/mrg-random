# mrg-random
This package contains pure haskell implementation of some
MRG (Multiple Recursive Generator) pseudo random number generators.

MRG PRNGs(`MRG32k3a`, `MRG63k3a`) have good features as following.

  * Long period (MRG32k3a: ≈ 2^191, MRG63k3a: ≈ 2^{377})
  * Small state vector (6 elements of `Double` / `Word64`)
  * Able to extract substream at small cost.
