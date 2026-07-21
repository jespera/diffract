package com.example.alpha

import com.example.testing.Suite

@Suite
object AlphaSuite {
    fun checkParser(input: Source, clock: Clock): Report =
        Report(input, clock)
}
