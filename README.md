# statsd-rupp

A [StatsD](https://github.com/statsd/statsd) client library for Haskell.

It can report individual samples, like a simple StatsD client, and it can
also aggregate samples in memory and send statistics periodically, like a
StatsD server. The second mode lets you send counts, rates, means, medians
and percentiles to any backend that speaks the StatsD line protocol, even if
it does not aggregate by itself.

Metrics are sent over UDP. Network errors are ignored, so a missing or slow
StatsD server never breaks your program.

## Usage

Add `statsd-rupp` to your dependencies and import `System.Metrics.StatsD`.

```haskell
import System.Metrics.StatsD

main :: IO ()
main = do
    let cfg = defStatConfig { namespace = "myapp", statsdServer = "statsd.local" }
    withStats cfg $ \stats -> do
        requests <- newStatCounter stats "requests" 1    -- send every sample
        latency  <- newStatTiming  stats "latency" 10    -- send 1 in 10 samples
        users    <- newStatGauge   stats "users" 0       -- initial value 0
        visitors <- newStatSet     stats "visitors"

        incrementCounter requests 1
        addTiming latency 42
        incrementGauge users 1
        newSetElement visitors "alice"
```

### Metric types

| Type          | Create with      | Update with                                          |
|---------------|------------------|------------------------------------------------------|
| Counter       | `newStatCounter` | `incrementCounter`                                   |
| Gauge         | `newStatGauge`   | `setGauge`, `incrementGauge`, `decrementGauge`       |
| Timing        | `newStatTiming`  | `addTiming`                                          |
| Set           | `newStatSet`     | `newSetElement`                                      |

Counters and timings take a sampling rate `n`: one in every `n` samples is
sent, tagged with rate `1/n`. Use `1` to send all of them, or `0` to send
none. Aggregated statistics always include every sample.

Keys may only contain ASCII letters, digits, `.`, `_` and `-`. Registering
the same key twice throws an `IOError`.

### Configuration

| Field               | Default       | Meaning                                              |
|---------------------|---------------|------------------------------------------------------|
| `reportStats`       | `True`        | Aggregate and send statistics every flush interval.  |
| `reportSamples`     | `True`        | Send every (sampled) reading as it happens.          |
| `namespace`         | `""`          | Prefix for all metric names.                         |
| `prefixStats`       | `"stats"`     | Prefix for aggregated statistics.                    |
| `prefixCounter`     | `"counters"`  | Prefix for counter statistics.                       |
| `prefixTimer`       | `"timers"`    | Prefix for timing statistics.                        |
| `prefixGauge`       | `"gauges"`    | Prefix for gauge statistics.                         |
| `prefixSet`         | `"sets"`      | Prefix for set statistics.                           |
| `statsdServer`      | `"127.0.0.1"` | StatsD host.                                         |
| `statsdPort`        | `8125`        | StatsD UDP port.                                     |
| `flushInterval`     | `1000`        | Milliseconds between flushes.                        |
| `timingPercentiles` | `[90, 95]`    | Percentiles computed for timings.                    |
| `appendNewline`     | `False`       | End each line with a newline.                        |

An invalid configuration (bad prefix, non-positive flush interval,
percentile outside 1–99) makes `withStats` fail.

### Metric names

With `namespace = "myapp"` and the default prefixes:

| What                | Names                                                          |
|---------------------|----------------------------------------------------------------|
| Samples             | `myapp.<key>`                                                  |
| Counter statistics  | `myapp.stats.counters.<key>.count`, `.rate` (per second)       |
| Gauge statistics    | `myapp.stats.gauges.<key>`                                     |
| Set statistics      | `myapp.stats.sets.<key>.count`                                 |
| Timing statistics   | `myapp.stats.timers.<key>.<stat>`                              |

Timing statistics are `count`, `count_ps`, `std`, `mean`, `upper`, `lower`,
`sum`, `sum_squares` and `median`. For each percentile `N`, the same
statistics except `count_ps` and `std` are also sent with a `_N` suffix,
computed over the lowest `N`% of the timings.

## Building and testing

```sh
stack build
stack test
```

## License

BSD 3-Clause. See [LICENSE](LICENSE).
