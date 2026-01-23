from builtin import print, assert
from bridge import run_mc_stats


fn test_mean_converges():
    let paths = 20000
    let seed = 42

    let (m50, _)   = run_mc_stats(paths, 50, seed)
    let (m500, _)  = run_mc_stats(paths, 500, seed)
    let (m5000, _) = run_mc_stats(paths, 5000, seed)

    assert(abs(m500)  < abs(m50))
    assert(abs(m5000) < abs(m500))


fn test_variance_positive():
    let (_, var) = run_mc_stats(10000, 200, 7)
    assert(var > 1e-6)


fn test_mc_noise_scaling():
    let steps = 500
    let seed = 99

    let (m1, _) = run_mc_stats(5000, steps, seed)
    let (m2, _) = run_mc_stats(20000, steps, seed)

    assert(abs(m2) < 0.75 * abs(m1))


fn main():
    test_mean_converges()
    test_variance_positive()
    test_mc_noise_scaling()
    print("All Monte Carlo tests passed.")
