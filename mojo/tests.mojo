from bridge import hedge_path, simulate_path

fn approx(a: Float64, b: Float64, eps: Float64 = 1e-6) -> Bool:
    return abs(a - b) < eps

fn test_zero_sigma():
    let path = [100.0, 100.0, 100.0, 100.0]
    let pnl = hedge_path(path, 100.0, 0.0, 0.0, 0.25)
    if not approx(pnl, 0.0):
        print("FAIL: zero sigma test")
        exit(1)

fn test_path_length():
    let path = simulate_path(100.0, 0.0, 0.2, 0.01, 50)
    if len(path) != 50:
        print("FAIL: path length")
        exit(1)

fn main():
    test_zero_sigma()
    test_path_length()
    print("Mojo tests passed")
