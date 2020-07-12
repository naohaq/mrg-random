# mrg-random
This package contains pure haskell implementation of some
MRG (Multiple Recursive Generator) pseudo random number generators.

MRG PRNGs(`MRG32k3a`, `MRG63k3a`) have good features as following.

  * Long period (`MRG32k3a`: ≈ 2¹⁹¹, `MRG63k3a`: ≈ 2³⁷⁷).
  * Generate high quality random numbers following U(0,1).
  * Small state vector (only 6 elements of `Double` / `Int64`).
  * Able to extract substream at small cost.

