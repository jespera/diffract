package com.example.gamma

import com.example.testing.Suite

@Suite
object GammaSuite {
    fun checkConversion(input: Source, clock: Clock): Report =
        Report(input, clock, retries = 3)
}

@Suite
class KeeperSuite {
    companion object {
        private val log = logger { }
    }

    fun checkKeeper(store: Store): Report {
        log.info("checking keeper")
        return Report(store)
    }
}
