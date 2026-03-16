#!/usr/bin/env python3
"""
Stand-alone script for time traces convergence analysis.

Reads a single CSV file with time traces readings, computes a set of
steady-state convergence metrics, and produces a convergence plot.

Metrics computed (per column / time series):
  - Autocorrelation Time  (ACT)  – via FFT-based ACF and exponential-decay fit
  - Effective sample size (n_eff = n_stationary / ACT)
  - Mean, Standard Deviation, Standard Error of the Mean (SEM = std / sqrt(n_eff))
  - Stationarity check    – linear-regression slope vs tolerance
  - Discontinuity check   – combined relative-second-difference and absolute-diff criterion
  - Per-window convergence – sequential estimation of ACT, mean, std, SEM over run windows

Usage:
    python time_traces_analysis.py <path_to_csv_file> [options]

The input CSV file should contain numeric columns (one time series per column).
A header row is auto-detected.  Columns can optionally be selected with --columns.

Examples:
    python time_traces_analysis.py traces.csv
    python time_traces_analysis.py traces.csv --columns 0 2 --alpha-discard 0.3
    python time_traces_analysis.py traces.csv --run-len 200 --output-prefix my_analysis

Packaged from uq/basicda/gem_da.py and uq/basicda/da_utils.py
(branch ets_json_params of the MFW repository).
"""

import argparse
import math
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy import interpolate
from scipy.stats import linregress


# ---------------------------------------------------------------------------
# Analysis helpers (extracted from da_utils.py and gem_da.py)
# ---------------------------------------------------------------------------

def compute_acf_fft(series):
    """Return the full autocorrelation function computed via FFT.

    Parameters
    ----------
    series : 1-D array-like
        Time series values (assumed equally spaced).

    Returns
    -------
    acfs : np.ndarray  – ACF values for lags 0 … N-1
    lags : np.ndarray  – corresponding lag indices
    """
    series = np.asarray(series, dtype=float)
    n = len(series)
    mean_val = series.mean()
    var_val = series.var()
    if var_val == 0.0:
        return np.ones(n), np.arange(n)

    val_shift = series - mean_val
    fft_size = int(2 ** np.ceil(np.log2(2 * n - 1)))
    cf = np.fft.fft(val_shift, fft_size)
    sf = cf.conjugate() * cf
    acfs = np.fft.ifft(sf).real[:n] / var_val / n
    lags = np.arange(n)
    return acfs, lags


def compute_act(series):
    """Compute the Autocorrelation Time (ACT) and effective sample size.

    Uses an FFT-based ACF followed by fitting  ACF(t) = exp(-t / tau)
    to the positive part of the ACF to obtain tau (the ACT).

    Parameters
    ----------
    series : 1-D array-like

    Returns
    -------
    act  : float – autocorrelation time (in time-step units)
    n_eff: int   – effective number of independent samples
    """
    series = np.asarray(series, dtype=float)
    n = len(series)
    if n < 4:
        return float(n), 1

    acfs, lags = compute_acf_fft(series)

    # Find the range of positive ACF values for the exponential fit
    n_positive = 0
    for idx, a in enumerate(acfs):
        if a <= 0.0:
            break
        n_positive = idx
    # Need at least two positive points for a regression
    if n_positive < 2:
        n_positive = 2

    # Fit  log(ACF(t)) = -t/tau  =>  slope = -1/tau
    with np.errstate(divide="ignore", invalid="ignore"):
        log_acf = np.log(np.clip(acfs[:n_positive], 1e-300, None))
    slope_res = linregress(lags[:n_positive].astype(float), log_acf)
    slope = slope_res.slope
    if slope >= 0:
        # ACF is not decaying – series may not be stationary at all
        act = float(n)
    else:
        act = -1.0 / slope

    act = max(act, 1.0)
    n_eff = max(int(n / act), 1)
    return act, n_eff


def stationarity_check(series):
    """Check approximate stationarity via linear regression.

    Fits Q = a·t + b and compares the slope *a* against a tolerance
    derived from the data range.

    Returns
    -------
    slope     : float – regression slope
    is_stationary : bool  – True when |slope| < tolerance
    a_tol     : float – the tolerance used
    """
    series = np.asarray(series, dtype=float)
    n = len(series)
    if n < 2:
        return 0.0, True, 0.0

    x = np.arange(n, dtype=float)
    res = linregress(x, series)
    a = res.slope

    alpha_tol = 5e-2
    data_range = series.max() - series.min()
    a_tol = alpha_tol * data_range / float(n) if n > 0 else 0.0

    return a, abs(a) < a_tol, a_tol


def discontinuity_check(series, reltol=5e-2, abstol=1e4):
    """Detect discontinuities in a time series.

    Uses a combined criterion of relative second differences and absolute
    first differences, following the logic in gem_da.py.

    Returns
    -------
    disc_indices : list[int] – indices at which discontinuities are detected
    n_disc       : int
    """
    series = np.asarray(series, dtype=float)
    n = len(series)
    if n < 4:
        return [], 0

    diff = series[1:] - series[:-1]
    second_diff = series[3:] - series[2:-1] - series[1:-2] + series[:-3]
    with np.errstate(divide="ignore", invalid="ignore"):
        rel_second_diff = np.where(
            series[1:-2] != 0,
            second_diff / series[1:-2],
            0.0,
        )

    ts1 = np.where(np.abs(rel_second_diff) > reltol)[0].tolist()
    ts1_set = set(ts1)
    ts1 = [t + 1 for t in ts1 if t + 2 in ts1_set]
    ts2 = np.where(np.abs(diff) > abstol)[0].tolist()

    ts1_filtered_set = set(ts1)
    ts = ts1 + [t for t in ts2 if t not in ts1_filtered_set]
    return sorted(set(ts)), len(ts)


def per_window_analysis(series, run_len=None, alpha_discard=0.3):
    """Run sequential per-window convergence analysis.

    Splits the stationary portion of *series* into windows of *run_len*
    time steps and computes local ACT, n_eff, mean, std, and SEM for each
    accumulating window (from the start of the stationary portion up to the
    end of the current window).

    Parameters
    ----------
    series        : 1-D array-like
    run_len       : int or None – window length; auto-chosen if None
    alpha_discard : float – fraction of initial ramp-up to discard

    Returns
    -------
    dict with keys:
        window_ends, lens, acts, acns, means, stds, sems,
        rel_mean_changes, abs_mean_changes,
        conv_nts_list, etol
    """
    series = np.asarray(series, dtype=float)
    n_tt = len(series)
    n_disc = math.floor(alpha_discard * n_tt)
    n_stat = n_tt - n_disc
    if n_stat < 4:
        return None

    if run_len is None:
        # Heuristic: choose ~10 windows
        run_len = max(n_stat // 10, 4)
    if run_len >= n_stat:
        run_len = n_stat
    n_r = math.floor(n_stat / run_len)
    if n_r < 1:
        return None

    lens = np.zeros(n_r)
    acts = np.zeros(n_r)
    acns = np.zeros(n_r)
    means = np.zeros(n_r)
    rel_mean_changes = np.zeros(n_r)
    abs_mean_changes = np.zeros(n_r)
    stds = np.zeros(n_r)
    sems = np.zeros(n_r)

    for i in range(n_r):
        i_l = n_disc + run_len * (i + 1)
        if i == n_r - 1:
            i_l = n_tt  # last window extends to the end

        traces_loc = series[n_disc:i_l]

        act_loc, acn_loc = compute_act(traces_loc)

        # ACF-corrected mean: take one reading per ACT window
        act_int = max(int(act_loc), 1)
        n_acf_samples = max(len(traces_loc) // act_int, 1)
        traces_acf_loc = np.array(
            [traces_loc[k * act_int : (k + 1) * act_int].mean() for k in range(n_acf_samples)]
        )

        lens[i] = len(traces_loc)
        acns[i] = acn_loc
        acts[i] = act_loc

        avg_loc = traces_acf_loc.mean()
        std_loc = traces_acf_loc.std()
        sem_loc = std_loc / np.sqrt(max(acn_loc, 1))

        means[i] = avg_loc
        stds[i] = std_loc
        sems[i] = sem_loc
        if i > 0 and means[i - 1] != 0:
            rel_mean_changes[i] = abs(means[i] - means[i - 1]) / abs(means[i - 1])
        else:
            rel_mean_changes[i] = 1.0
        abs_mean_changes[i] = abs(means[i] - means[i - 1]) if i > 0 else 0.0

    # ---- Convergence criterion: relative SEM < tolerance ----
    window_ends = [n_disc + run_len * (i + 1) for i in range(n_r)]
    window_ends[-1] = n_tt

    n_an_steps = 128
    etol = np.logspace(-3, 0.0, n_an_steps)
    conv_nts_list = [None] * n_an_steps

    for j, et in enumerate(etol):
        for i in range(n_r):
            if means[i] != 0 and sems[i] / abs(means[i]) < et:
                conv_nts_list[j] = window_ends[i]
                break
        if conv_nts_list[j] is None:
            conv_nts_list[j] = n_tt

    return {
        "window_ends": np.array(window_ends),
        "lens": lens,
        "acts": acts,
        "acns": acns,
        "means": means,
        "stds": stds,
        "sems": sems,
        "rel_mean_changes": rel_mean_changes,
        "abs_mean_changes": abs_mean_changes,
        "conv_nts_list": conv_nts_list,
        "etol": etol,
        "n_disc": n_disc,
        "run_len": run_len,
        "n_r": n_r,
    }


# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

def plot_convergence(series, pw, col_name, output_prefix):
    """Produce a two-panel convergence plot for a single time series.

    Top panel:  raw time traces with per-window mean / ±SEM / ±STD bands.
    Bottom panel: evolution of ACT, SEM, mean, and STD over windows.
    Also saves a separate convergence-criterion plot.
    """
    series = np.asarray(series, dtype=float)
    n_tt = len(series)
    n_disc = pw["n_disc"]
    run_len = pw["run_len"]
    n_r = pw["n_r"]

    y_min, y_max = series.min(), series.max()
    margin = 0.1 * max(abs(y_min), abs(y_max), 1.0)
    y_lim = (y_min - margin, y_max + margin)

    fig, ax = plt.subplots(2, 1, figsize=(14, 10), gridspec_kw={"height_ratios": [3, 2]})

    # ---- Top panel: time traces with statistics ----
    # Ramp-up phase
    ax[0].plot(np.arange(0, n_disc), series[:n_disc], color="b", linewidth=0.7, alpha=0.6)
    ax[0].axvline(n_disc, color="grey", alpha=0.5, linestyle="--", label="start of stationary phase")

    for i in range(n_r):
        i_f = n_disc + run_len * i
        i_l = pw["window_ends"][i]
        x_range = np.arange(i_f, min(i_l + 1, n_tt))
        ax[0].plot(x_range, series[i_f : i_f + len(x_range)], color="b", linewidth=0.7)
        if i < n_r - 1:
            ax[0].axvline(i_l, color="grey", alpha=0.2, linestyle="--")

        avg = pw["means"][i]
        std = pw["stds"][i]
        sem = pw["sems"][i]
        ax[0].hlines(y=avg, xmin=i_f, xmax=i_l, color="g", linewidth=1.2)
        ax[0].hlines(y=avg + sem, xmin=i_f, xmax=i_l, color="g", linestyle="--", linewidth=0.8)
        ax[0].hlines(y=avg - sem, xmin=i_f, xmax=i_l, color="g", linestyle="--", linewidth=0.8)
        ax[0].hlines(y=avg + 1.96 * std, xmin=i_f, xmax=i_l, color="g", linestyle="dotted", linewidth=0.8)
        ax[0].hlines(y=avg - 1.96 * std, xmin=i_f, xmax=i_l, color="g", linestyle="dotted", linewidth=0.8)

    # Add legend entries (only once)
    last_avg = pw["means"][-1]
    last_sem = pw["sems"][-1]
    last_std = pw["stds"][-1]
    ax[0].hlines(y=[], xmin=0, xmax=0, color="g", linewidth=1.2, label=f"mean (last: {last_avg:.4g})")
    ax[0].hlines(y=[], xmin=0, xmax=0, color="g", linestyle="--", linewidth=0.8, label=f"±SEM (last: {last_sem:.4g})")
    ax[0].hlines(y=[], xmin=0, xmax=0, color="g", linestyle="dotted", linewidth=0.8, label=f"±1.96·STD (last: {last_std:.4g})")

    ax[0].set_ylabel("Value")
    ax[0].set_xlabel("Time step")
    ax[0].set_ylim(*y_lim)
    ax[0].set_title(f"Time traces convergence – {col_name}")
    ax[0].legend(loc="best", fontsize=8, framealpha=0.6)
    ax[0].ticklabel_format(axis="y", style="sci", scilimits=(-3, 4), useMathText=True)

    # ---- Bottom panel: metric evolution ----
    x_windows = pw["window_ends"]

    color_act = "tab:blue"
    ax[1].plot(x_windows, pw["acts"], color=color_act, marker="o", markersize=3, label="ACT (time steps)")
    ax[1].set_ylabel("ACT (time steps)", color=color_act)
    ax[1].set_xlabel("Time step (end of window)")
    ax[1].tick_params(axis="y", labelcolor=color_act)

    ax_sem = ax[1].twinx()
    color_sem = "tab:green"
    ax_sem.plot(x_windows, pw["sems"], color=color_sem, marker="s", markersize=3, label="SEM")
    ax_sem.set_ylabel("SEM", color=color_sem)
    ax_sem.tick_params(axis="y", labelcolor=color_sem)
    ax_sem.ticklabel_format(axis="y", style="sci", scilimits=(-3, 4), useMathText=True)

    # Add mean on the primary axis (scaled)
    ax[1].plot(x_windows, pw["means"] / max(abs(pw["means"]).max(), 1e-30) * pw["acts"].max() * 0.5,
               color="tab:red", marker="^", markersize=3, linestyle="--", label="Mean (scaled)")
    ax[1].plot(x_windows, pw["stds"] / max(abs(pw["stds"]).max(), 1e-30) * pw["acts"].max() * 0.3,
               color="tab:orange", marker="v", markersize=3, linestyle=":", label="STD (scaled)")

    ax[1].legend(loc="upper left", fontsize=7, framealpha=0.6)
    ax_sem.legend(loc="upper right", fontsize=7, framealpha=0.6)

    fig.tight_layout()
    fname = f"{output_prefix}_convergence_{col_name}.png"
    fig.savefig(fname, dpi=150)
    plt.close(fig)
    print(f"  Saved convergence plot: {fname}")

    # ---- Separate convergence-criterion plot ----
    fig_c, ax_c = plt.subplots(figsize=(7, 5))
    ax_c.plot(pw["etol"], pw["conv_nts_list"], color="b", marker="o", markersize=2)
    ax_c.set_xscale("log")
    ax_c.set_yscale("log")
    ax_c.set_xlabel("Relative SEM tolerance (ε)")
    ax_c.set_ylabel("Time steps to convergence")
    ax_c.set_title(f"Convergence criterion – {col_name}")
    ax_c.grid(True, which="both", alpha=0.3)
    fig_c.tight_layout()
    fname_c = f"{output_prefix}_conv_criterion_{col_name}.png"
    fig_c.savefig(fname_c, dpi=150)
    plt.close(fig_c)
    print(f"  Saved convergence criterion plot: {fname_c}")


# ---------------------------------------------------------------------------
# Main analysis pipeline
# ---------------------------------------------------------------------------

def analyse_single_series(series, col_name, alpha_discard, run_len, output_prefix):
    """Run the full analysis pipeline on a single time series."""
    series = np.asarray(series, dtype=float)
    n = len(series)
    print(f"\n{'=' * 60}")
    print(f"Column: {col_name}  ({n} time steps)")
    print(f"{'=' * 60}")

    # 1. Discard ramp-up
    n_disc = math.floor(alpha_discard * n)
    stationary = series[n_disc:]
    n_stat = len(stationary)
    print(f"  Discarding first {n_disc} steps ({alpha_discard:.0%} ramp-up).")
    print(f"  Stationary portion: {n_stat} steps.")

    # 2. Discontinuity check
    disc_idx, n_discs = discontinuity_check(stationary)
    if n_discs > 0:
        print(f"  WARNING: {n_discs} discontinuities detected at indices (relative to stationary start): {disc_idx[:20]}{'...' if n_discs > 20 else ''}")
    else:
        print(f"  No discontinuities detected.")

    # 3. ACT and effective sample size
    act, n_eff = compute_act(stationary)
    print(f"  Autocorrelation Time (ACT): {act:.2f} time steps")
    print(f"  Effective sample size (n_eff): {n_eff}")

    # 4. Basic statistics (ACF-corrected)
    act_int = max(int(act), 1)
    n_acf_samples = max(n_stat // act_int, 1)
    acf_means = np.array(
        [stationary[k * act_int : (k + 1) * act_int].mean() for k in range(n_acf_samples)]
    )
    mean_val = acf_means.mean()
    std_val = acf_means.std()
    sem_val = std_val / np.sqrt(max(n_eff, 1))
    print(f"  Mean (ACF-corrected):  {mean_val:.6g}")
    print(f"  Std  (ACF-corrected):  {std_val:.6g}")
    print(f"  SEM  (ACF-corrected):  {sem_val:.6g}")
    if mean_val != 0:
        print(f"  Relative SEM:          {abs(sem_val / mean_val):.6g}")

    # 5. Stationarity check (linear regression)
    slope, is_stationary, a_tol = stationarity_check(stationary)
    status = "STATIONARY" if is_stationary else "NON-STATIONARY"
    print(f"  Linear-regression slope: {slope:.6g}  (tolerance: {a_tol:.6g})  → {status}")

    # 6. Per-window convergence analysis & plotting
    pw = per_window_analysis(series, run_len=run_len, alpha_discard=alpha_discard)
    if pw is not None:
        plot_convergence(series, pw, col_name, output_prefix)
    else:
        print("  Series too short for per-window analysis.")

    # 7. Summary verdict
    print(f"\n  --- Steady-state likelihood summary for '{col_name}' ---")
    score = 0
    reasons = []

    if is_stationary:
        score += 1
        reasons.append("Linear trend is within tolerance (PASS)")
    else:
        reasons.append("Linear trend exceeds tolerance (FAIL)")

    if n_discs == 0:
        score += 1
        reasons.append("No discontinuities detected (PASS)")
    else:
        reasons.append(f"{n_discs} discontinuities detected (FAIL)")

    if n_eff >= 10:
        score += 1
        reasons.append(f"Effective sample size {n_eff} >= 10 (PASS)")
    else:
        reasons.append(f"Effective sample size {n_eff} < 10 (FAIL)")

    if mean_val != 0 and abs(sem_val / mean_val) < 0.05:
        score += 1
        reasons.append(f"Relative SEM {abs(sem_val / mean_val):.4g} < 5% (PASS)")
    elif mean_val != 0:
        reasons.append(f"Relative SEM {abs(sem_val / mean_val):.4g} >= 5% (FAIL)")
    else:
        reasons.append("Mean is zero – relative SEM undefined (WARN)")

    if pw is not None:
        last_rmc = pw["rel_mean_changes"][-1]
        if last_rmc < 0.02:
            score += 1
            reasons.append(f"Last relative mean change {last_rmc:.4g} < 2% (PASS)")
        else:
            reasons.append(f"Last relative mean change {last_rmc:.4g} >= 2% (FAIL)")

    max_score = 5
    for r in reasons:
        print(f"    • {r}")
    print(f"  Score: {score}/{max_score}  –  ", end="")
    if score >= 4:
        print("HIGH likelihood of steady state ✓")
    elif score >= 2:
        print("MODERATE likelihood of steady state ~")
    else:
        print("LOW likelihood of steady state ✗")

    return {
        "column": col_name,
        "n_total": n,
        "n_stationary": n_stat,
        "act": act,
        "n_eff": n_eff,
        "mean": mean_val,
        "std": std_val,
        "sem": sem_val,
        "relative_sem": abs(sem_val / mean_val) if mean_val != 0 else float("inf"),
        "slope": slope,
        "is_stationary": is_stationary,
        "n_discontinuities": n_discs,
        "score": score,
        "max_score": max_score,
    }


def read_time_traces(filepath):
    """Read a CSV file with time traces.

    Supports files with or without headers.  Returns a DataFrame whose
    columns are the individual time series.
    """
    # Try to detect whether the first row is a header or data
    try:
        df_test = pd.read_csv(filepath, nrows=2, header=None, sep=None, engine="python")
    except FileNotFoundError:
        sys.exit(f"Error: file not found: '{filepath}'")
    except pd.errors.ParserError as exc:
        sys.exit(f"Error parsing CSV file '{filepath}': {exc}")
    except (ValueError, UnicodeDecodeError) as exc:
        sys.exit(f"Error reading file '{filepath}': {exc}")

    first_row_numeric = all(
        isinstance(v, (int, float, np.integer, np.floating)) for v in df_test.iloc[0]
    )

    if first_row_numeric:
        df = pd.read_csv(filepath, header=None, sep=None, engine="python")
        df.columns = [f"col_{i}" for i in range(df.shape[1])]
    else:
        df = pd.read_csv(filepath, sep=None, engine="python")

    # Drop any fully-NaN columns
    df = df.dropna(axis=1, how="all")
    # Keep only numeric columns
    df = df.select_dtypes(include=[np.number])

    if df.empty:
        sys.exit(f"Error: no numeric columns found in '{filepath}'.")

    return df


def main():
    parser = argparse.ArgumentParser(
        description="Time traces convergence analysis – computes steady-state metrics and produces convergence plots.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("filepath", help="Path to a CSV file containing time traces (one series per column).")
    parser.add_argument(
        "--columns",
        nargs="*",
        default=None,
        help="Columns to analyse (by 0-based index or name). Default: all numeric columns.",
    )
    parser.add_argument(
        "--alpha-discard",
        type=float,
        default=0.3,
        help="Fraction of initial readings to discard as ramp-up (default: 0.3).",
    )
    parser.add_argument(
        "--run-len",
        type=int,
        default=None,
        help="Window length (time steps) for per-window analysis. Default: auto (~10 windows).",
    )
    parser.add_argument(
        "--output-prefix",
        default=None,
        help="Prefix for output plot files. Default: derived from input filename.",
    )

    args = parser.parse_args()

    filepath = args.filepath
    if not os.path.isfile(filepath):
        sys.exit(f"Error: file not found: '{filepath}'")

    if args.output_prefix is None:
        args.output_prefix = os.path.splitext(os.path.basename(filepath))[0]

    print(f"Reading time traces from: {filepath}")
    df = read_time_traces(filepath)
    print(f"  Found {df.shape[1]} numeric column(s), {df.shape[0]} rows.")

    # Select columns
    if args.columns is not None:
        selected = []
        for c in args.columns:
            try:
                idx = int(c)
                selected.append(df.columns[idx])
            except (ValueError, IndexError):
                if c in df.columns:
                    selected.append(c)
                else:
                    sys.exit(f"Error: column '{c}' not found. Available: {list(df.columns)}")
        df = df[selected]

    results = []
    for col in df.columns:
        series = df[col].dropna().values
        if len(series) < 4:
            print(f"\nSkipping column '{col}': too few data points ({len(series)}).")
            continue
        res = analyse_single_series(
            series,
            col_name=str(col),
            alpha_discard=args.alpha_discard,
            run_len=args.run_len,
            output_prefix=args.output_prefix,
        )
        results.append(res)

    # Save summary CSV
    if results:
        summary_df = pd.DataFrame(results)
        summary_file = f"{args.output_prefix}_summary.csv"
        summary_df.to_csv(summary_file, index=False)
        print(f"\nSummary saved to: {summary_file}")

    print("\nDone.")


if __name__ == "__main__":
    main()
