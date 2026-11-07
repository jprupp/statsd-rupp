# statsd-rupp

[![Hackage](https://img.shields.io/hackage/v/statsd-rupp.svg)](https://hackage.haskell.org/package/statsd-rupp)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://github.com/jprupp/statsd-rupp/blob/master/LICENSE)

statsd-rupp is a [StatsD](https://github.com/statsd/statsd) client for Haskell.

It works in two ways, which you can use separately or together:

- **As a client**: it sends each sample to the StatsD server as it happens, with
  optional sampling.
- **As an aggregator**: it keeps samples in memory and every flush interval sends
  statistics, such as counts, rates, means, medians and percentiles, like a
  StatsD server does. This lets you feed backends that speak the StatsD line
  protocol but do not aggregate, such as some Graphite and InfluxDB relays.

Metrics go over UDP and send errors are ignored, so a missing StatsD server
never breaks your program.

- [API documentation](https://hackage.haskell.org/package/statsd-rupp)
- [Changelog](https://github.com/jprupp/statsd-rupp/blob/master/CHANGELOG.md)
- [Report a bug](https://github.com/jprupp/statsd-rupp/issues)

## Contents

- [Features](#features)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Concepts](#concepts)
- [Usage guide](#usage-guide)
- [Configuration](#configuration)
- [Metric names](#metric-names)
- [Module overview](#module-overview)
- [Development](#development)
- [Related projects](#related-projects)
- [License](#license)

## Features

| Area         | What you get                                                                     |
| ------------ | -------------------------------------------------------------------------------- |
| Metric types | Counters, gauges, timings and sets                                               |
| Samples      | Each reading sent as it happens, with per-metric sampling rates                  |
| Aggregation  | Counts and rates; gauge values; distinct set sizes; timing count, rate, mean, median, standard deviation, minimum, maximum, sum, sum of squares and percentiles |
| Naming       | Configurable namespace and prefixes; key validation                              |
| Transport    | UDP to any host and port, with an optional newline after each line               |
| Monads       | Works in any `MonadIO`; `withStats` needs `MonadUnliftIO`                         |

### What is not included

- No tags (the DogStatsD extension) and no histogram or distribution types.
- No TCP transport and no batching of several lines in one packet.
- No server: this library only sends.

## Installation

statsd-rupp is on [Hackage](https://hackage.haskell.org/package/statsd-rupp) and
in Stackage snapshots. It has no C dependencies. It needs GHC 9.2 or later,
because it uses `OverloadedRecordDot` and `NoFieldSelectors`.

### With Cabal

```cabal
build-depends:
    base
  , statsd-rupp
```

### With Stack

```yaml
# package.yaml
dependencies:
  - statsd-rupp
```

If your resolver does not include it, add it to `stack.yaml`:

```yaml
extra-deps:
  - statsd-rupp-0.5.0.1
```

## Quick start

```haskell
import System.Metrics.StatsD

main :: IO ()
main = do
  let cfg = defStatConfig {namespace = "myapp", statsdServer = "statsd.local"}
  withStats cfg $ \stats -> do
    requests <- newStatCounter stats "requests" 1 -- send every sample
    latency <- newStatTiming stats "latency" 10 -- send 1 sample in 10
    users <- newStatGauge stats "users" 0 -- start at 0
    visitors <- newStatSet stats "visitors"

    incrementCounter requests 1
    addTiming latency 42
    incrementGauge users 1
    newSetElement visitors "alice"
```

With the default configuration this sends `myapp.requests:1|c`,
`myapp.users:+1|g` and `myapp.visitors:alice|s` right away. Every second it
sends `myapp.stats.counters.requests.count`, `myapp.stats.timers.latency.mean`
and the other statistics listed under [Metric names](#metric-names).

## Concepts

### The client handle

`withStats` resolves the server address, opens a UDP socket, and passes a
`Stats` handle to your function. If aggregation is on, it also runs a thread
that flushes statistics. Everything stops when the function returns. A `Stats`
can be shared between threads.

### Registering metrics

Each metric is registered once with a `newStat*` function, which returns a
handle to update it with. Keys may contain only ASCII letters, digits, `.`, `_`
and `-`. Registering a key twice, or an invalid key, throws an `IOError`.

### Sampling

Counters and timings take a sampling rate `n`. With samples turned on, one
reading in every `n` is sent, tagged with the rate `1/n` (`|@0.100000` for
`n = 10`) so that the server
can scale it back up. Use `1` to send every reading and `0` to send none.
Aggregated statistics always include every reading.

### Flushing

Every `flushInterval` milliseconds, the aggregated data of every metric is sent
and reset: counters go back to zero, timings and sets are emptied, gauges keep
their value.

## Usage guide

### Counters

```haskell
c <- newStatCounter stats "jobs.done" 1
incrementCounter c 1
incrementCounter c 5
```

### Gauges

```haskell
g <- newStatGauge stats "queue.length" 0
setGauge g 10
incrementGauge g 3
decrementGauge g 1
```

The aggregated value never drops below zero and saturates at `maxBound`. In the
StatsD protocol a negative value means "decrease", so `setGauge g (-1)` is seen
by a server as a decrement.

### Timings

```haskell
t <- newStatTiming stats "db.query" 1
start <- getCurrentTime
runQuery
end <- getCurrentTime
addTiming t (round (1000 * diffUTCTime end start))
```

Timings are whole numbers, normally milliseconds.

### Sets

```haskell
s <- newStatSet stats "active_users"
newSetElement s userId
```

The aggregated statistic is the number of distinct elements seen in the flush
interval.

### Samples only, or statistics only

```haskell
-- a plain StatsD client
defStatConfig {reportStats = False}

-- an aggregator that sends nothing until it flushes
defStatConfig {reportSamples = False}
```

## Configuration

Start from `defStatConfig` and override the fields you need.

| Field               | Default       | Meaning                                                    |
| ------------------- | ------------- | ---------------------------------------------------------- |
| `reportStats`       | `True`        | Aggregate and send statistics every flush interval         |
| `reportSamples`     | `True`        | Send each (sampled) reading as it happens                  |
| `namespace`         | `""`          | Prefix for all metric names                                |
| `prefixStats`       | `"stats"`     | Prefix for aggregated statistics, after the namespace      |
| `prefixCounter`     | `"counters"`  | Prefix for counter statistics                              |
| `prefixTimer`       | `"timers"`    | Prefix for timing statistics                               |
| `prefixGauge`       | `"gauges"`    | Prefix for gauge statistics                                |
| `prefixSet`         | `"sets"`      | Prefix for set statistics                                  |
| `statsdServer`      | `"127.0.0.1"` | StatsD host name or address                                |
| `statsdPort`        | `8125`        | StatsD UDP port                                            |
| `flushInterval`     | `1000`        | Milliseconds between flushes; must be positive             |
| `timingPercentiles` | `[90, 95]`    | Percentiles to compute for timings, each from 1 to 99      |
| `appendNewline`     | `False`       | End each line with a newline, as some servers need         |

`withStats` fails with `error` if a prefix or the namespace is not a valid key,
the flush interval is not positive, a percentile is out of range, or the server
name cannot be resolved.

## Metric names

With `namespace = "myapp"` and the default prefixes:

| What                | Names                                                         |
| ------------------- | ------------------------------------------------------------- |
| Samples             | `myapp.<key>`                                                 |
| Counter statistics  | `myapp.stats.counters.<key>.count`, `.rate` (per second)      |
| Gauge statistics    | `myapp.stats.gauges.<key>`                                    |
| Set statistics      | `myapp.stats.sets.<key>.count`                                |
| Timing statistics   | `myapp.stats.timers.<key>.<stat>`                             |

Timing statistics are `count`, `count_ps` (per second), `std`, `mean`, `upper`,
`lower`, `sum`, `sum_squares` and `median`. For each percentile `N`, the same
statistics except `count_ps` and `std` are also sent with an `_N` suffix,
computed over the lowest `N`% of the timings: for example `mean_90` and
`upper_95`.

Without a namespace, names start at `stats.` and samples at the key.

## Module overview

| Module                            | Contents                                                       |
| --------------------------------- | -------------------------------------------------------------- |
| `System.Metrics.StatsD`           | The public API: configuration, `withStats`, metric types       |
| `System.Metrics.StatsD.Internal`  | Aggregation, the line format and its parser; may change between minor versions |

## Development

```sh
git clone https://github.com/jprupp/statsd-rupp.git
cd statsd-rupp

stack build                        # build the library
stack test                         # run the tests
stack haddock --no-haddock-deps    # build the API documentation
```

`cabal build` and `cabal test` work too.

`package.yaml` is the source of the package description. Change it rather than
`statsd-rupp.cabal`, which [hpack](https://github.com/sol/hpack) generates.

The tests are in `test/Spec.hs` and use [hspec](https://hspec.github.io/) and
QuickCheck. They check the line format against its parser, the statistics
against simple reference implementations, and what each metric type sends,
through a local socket pair.

### Contributing

Pull requests are welcome.

- Format the code with [Ormolu](https://github.com/tweag/ormolu).
- Add tests for the behaviour you change.
- Describe the change in `CHANGELOG.md`, which follows
  [Keep a Changelog](https://keepachangelog.com/en/1.0.0/). Versions follow the
  [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Related projects

- [haskoin-store](https://github.com/jprupp/haskoin-store) reports its metrics
  with this library.

## License

statsd-rupp is released under the
[BSD 3-Clause License](https://github.com/jprupp/statsd-rupp/blob/master/LICENSE).
