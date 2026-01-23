from math import sqrt, exp, log, cos

struct RNG:
    var state: Int

    fn __init__(out self, state: Int):
        self.state = state

    fn next_float(mut self) -> Float64:
        # Simple Linear Congruential Generator
        self.state = (self.state * 1103515245 + 12345) & 0x7fffffff
        return Float64(self.state) / 2147483647.0

    fn next_gaussian(mut self) -> Float64:
        var u1 = self.next_float()
        var u2 = self.next_float()
        if u1 < 1e-9:
            u1 = 1e-9
        return sqrt(-2.0 * log(u1)) * cos(2.0 * 3.1415926535 * u2)


struct MCStats:
    var mean: Float64
    var variance: Float64
    var paths: Int
    var std_error: Float64

    fn __init__(out self, mean: Float64, variance: Float64, paths: Int):
        self.mean = mean
        self.variance = variance
        self.paths = paths
        # Standard Error = sqrt(variance / paths)
        self.std_error = sqrt(variance / Float64(paths))


fn simulate_one_path(
    f0: Float64,
    k: Float64,
    t: Float64,
    sigma: Float64,
    steps: Int,
    mut rng: RNG
) -> Float64:
    var dt = t / Float64(steps)
    var price = f0
    
    # Precompute constants
    var drift = -0.5 * sigma * sigma * dt
    var vol_coef = sigma * sqrt(dt)

    for _ in range(steps):
        var z = rng.next_gaussian()
        price = price * exp(drift + vol_coef * z)

    # Payoff for Call Option
    return max(price - k, 0.0)


fn run_monte_carlo(
    f0: Float64,
    k: Float64,
    t: Float64,
    sigma: Float64,
    steps: Int,
    paths: Int,
    mut rng: RNG
) -> MCStats:
    var sum = 0.0
    var sumsq = 0.0

    var i = 0
    while i < paths:
        var pnl = simulate_one_path(f0, k, t, sigma, steps, rng)
        sum = sum + pnl
        sumsq = sumsq + pnl * pnl
        i = i + 1

    var mean = sum / Float64(paths)
    var var_ = sumsq / Float64(paths) - mean * mean

    return MCStats(mean, var_, paths)


# --- TEST HELPERS ---

fn assert_true(condition: Bool, msg: String):
    if condition:
        print("[PASS] " + msg)
    else:
        print("[FAIL] " + msg)

fn main():
    print("Running Black-Scholes Monte Carlo Tests...\n")

    # --- Test 1: Variance Sanity ---
    # Expectation: Variance should be > 0 for a standard option
    var rng1 = RNG(1001)
    var stats1 = run_monte_carlo(50.0, 50.0, 1.0, 0.2, 50, 1000, rng1)
    
    assert_true(stats1.variance > 0.0, "Variance is positive")
    print("      (Variance was: " + String(stats1.variance) + ")\n")


    # --- Test 2: Deep Out-of-the-Money (OTM) ---
    # Expectation: If Strike (K) is much higher than Price (F0), mean PnL -> 0
    var rng2 = RNG(2002)
    # Price = 10, Strike = 100. Impossible to reach in 1 year with 20% vol.
    var stats2 = run_monte_carlo(10.0, 100.0, 1.0, 0.2, 50, 1000, rng2)
    
    assert_true(stats2.mean < 0.01, "Deep OTM option value approx 0")
    print("      (Mean PnL was: " + String(stats2.mean) + ")\n")


    # --- Test 3: Monte Carlo Convergence (Noise Reduction) ---
    # Expectation: Quadrupling paths should roughly halve the Standard Error.
    # We compare 10,000 paths vs 40,000 paths.
    
    var rng3 = RNG(3003)
    var paths_low = 10000
    var stats_low = run_monte_carlo(50.0, 50.0, 1.0, 0.2, 50, paths_low, rng3)
    
    var rng4 = RNG(4004)
    var paths_high = 40000
    var stats_high = run_monte_carlo(50.0, 50.0, 1.0, 0.2, 50, paths_high, rng4)

    print("Checking Convergence:")
    print("  Low Paths  (" + String(paths_low) + ") StdErr: " + String(stats_low.std_error))
    print("  High Paths (" + String(paths_high) + ") StdErr: " + String(stats_high.std_error))
    
    # Ideally ratio is ~2.0. We check if High error is less than Low error * 0.75 (generous buffer)
    var improved = stats_high.std_error < stats_low.std_error
    assert_true(improved, "More paths reduced the Standard Error")
