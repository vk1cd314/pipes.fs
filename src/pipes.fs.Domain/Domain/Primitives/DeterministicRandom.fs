namespace pipes.fs.Domain.Primitives

open pipes.fs.Domain.Types

[<RequireQualifiedAccess>]
module DeterministicRandom =
    let private splitMixGamma = 0x9E3779B97F4A7C15UL

    // SplitMix64 reference implementation and constants:
    // https://prng.di.unimi.it/splitmix64.c
    let nextUInt64 (deterministicRandomState: DeterministicRandomState) =
        let nextDeterministicRandomState = deterministicRandomState + splitMixGamma
        let firstMixedValue              = (nextDeterministicRandomState ^^^ (nextDeterministicRandomState >>> 30)) * 0xBF58476D1CE4E5B9UL

        let secondMixedValue = (firstMixedValue ^^^ (firstMixedValue >>> 27)) * 0x94D049BB133111EBUL
        let generatedValue   = secondMixedValue ^^^ (secondMixedValue >>> 31)
        generatedValue, nextDeterministicRandomState

    // Uniform float32 in [0, 1) via top 24 bits
    // https://docs.oracle.com/en/java/javase/24/docs/api/java.base/java/util/Random.html#nextFloat()
    let nextFloat01 (deterministicRandomState: DeterministicRandomState) =
        let generatedUInt64Value, nextDeterministicRandomState = nextUInt64 deterministicRandomState
        let top24BitMantissa                                   = int (generatedUInt64Value >>> 40)
        ((top24BitMantissa |> float32) / 16777216.0f), nextDeterministicRandomState

    // NOTE: Modulo reduction is simple but can introduce bounded-range bias when maxExclusive does not divide 2^64:
    // https://www.pcg-random.org/posts/bounded-rands.html
    let nextInt (maxExclusive: int) (deterministicRandomState: DeterministicRandomState) =
        if maxExclusive <= 0 then
            invalidArg "maxExclusive" "maxExclusive must be > 0."

        let generatedUInt64Value, nextDeterministicRandomState = nextUInt64 deterministicRandomState
        generatedUInt64Value % (maxExclusive |> uint64) |> int, nextDeterministicRandomState

    let nextBoolWithProbability (probability: float32) (deterministicRandomState: DeterministicRandomState) =
        let randomSample, nextDeterministicRandomState = nextFloat01 deterministicRandomState
        randomSample < probability, nextDeterministicRandomState
