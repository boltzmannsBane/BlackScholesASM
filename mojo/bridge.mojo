from math import log, sqrt, exp, max
from random import normal
from json import loads, dumps
from fs import read_text, write_text

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

fn mean(xs: List[Float64]) -> Float64:
    var s = 0.0
    for x in xs:
        s += x
    return s / Float64(len(xs))

fn std(xs: List[Float64], m: Float64) -> Float64:
    var s = 0.0
    for x in xs:
        let d = x - m
        s += d * d
    return sqrt(s / Float64(len(xs)))

fn main():
    let input = loads(read_text("input.json"))

    let s0    = input["s0"]
    let mu    = input["mu"]
    let sigma = input["sigma"]
    let r     = input["r"]
    let k     = input["k"]
    let steps = Int(input["steps"])
    let paths = Int(input["paths"])

    let dt = 1.0 / Float64(steps)

    var pnls = List[Float64]()

    for i in range(paths):
        let path = simulate_path(s0, mu, sigma, dt, steps)
        let pnl = hedge_path(path, k, r, sigma, dt)
        pnls.append(pnl)

    pnls.sort()

    let idx = Int(0.01 * Float64(paths))
    let var99 = pnls[idx]

    var tail_sum = 0.0
    for i in range(idx):
        tail_sum += pnls[i]
    let cvar99 = tail_sum / Float64(idx)

    let m = mean(pnls)
    let sd = std(pnls, m)

    let output = {
        "var_99": var99,
        "cvar_99": cvar99,
        "mean": m,
        "std": sd
    }

    write_text("output.json", dumps(output))
