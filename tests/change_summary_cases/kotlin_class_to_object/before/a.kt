package com.example.alpha

import com.example.testing.Suite

@Suite
class AlphaSuite {
    fun checkParser(input: Source, clock: Clock): Report =
        Report(input, clock)
}
