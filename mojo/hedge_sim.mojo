from math import log, sqrt, exp, max
from random import normal
from sys import argv

fn norm_cdf(x: Float64) -> Float64:
    return 0.5 * (1.0 + erf(x / sqrt(2.0)))

fn black76_delta_call(f: Float64, k: Float64, r: Float64, t: Float64, sigma: Float64) -> Float64:
    let d1 = (log(f / k) + 0.5 * sigma * sigma * t) / (sigma * sqrt(t))
    return exp(-r * t) * norm_cdf(d1)

fn simulate_path(s0: Float64, mu: Float64, sigma: Float64, dt: Float64, n: Int) -> List[Float64]:
    var s = s0
    var path = List[Float64]()
    for i in range(n):
        let z = normal()
        s = s * exp((mu - 0.5 * sigma * sigma) * dt + sigma * sqrt(dt) * z)
        path.append(s)
    return path

fn hedge_path(path: List[Float64], k: Float64, r: Float64, sigma: Float64, dt: Float64) -> Float64:
    let n = len(path)
    var cash: Float64 = 0.0
    var delta: Float64 = 0.0

    for i in range(n):
        let t = dt * Float64(n - i)
        let s = path[i]
        let new_delta = black76_delta_call(s, k, r, t, sigma)
        let ddelta = new_delta - delta
        cash -= ddelta * s
        delta = new_delta

    let final_s = path[n-1]
    let payoff = max(0.0, final_s - k)
    return cash + delta * final_s - payoff

fn main():
    let paths = 100_000
    let steps = 252
    let s0 = 100.0
    let mu = 0.0
    let sigma = 0.2
    let r = 0.01
    let k = 100.0
    let dt = 1.0 / 252.0

    var pnls = List[Float64]()

    for i in range(paths):
        let path = simulate_path(s0, mu, sigma, dt, steps)
        let pnl = hedge_path(path, k, r, sigma, dt)
        pnls.append(pnl)

    pnls.sort()
    let var99 = pnls[Int(0.01 * Float64(paths))]
    let cvar99 = sum(pnls[0:Int(0.01 * Float64(paths))]) / (0.01 * Float64(paths))

    print("VaR 99%:", var99)
    print("CVaR 99%:", cvar99)
