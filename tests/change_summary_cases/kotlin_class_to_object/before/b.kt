package com.example.beta

import com.example.testing.Suite

@Suite
class BetaSuite {
    private val defaultLimit = 42

    fun checkRegistry(registry: Registry): Report = Report(registry)

    fun checkLimit(): Int = defaultLimit
}

@Suite(includes = [BetaSuite::class])
class BetaNightlySuite {
    fun checkAggregate(beta: Report): Aggregate = Aggregate(beta)
}
