package com.example.legacy

class Gamma {
    fun ping(): String = LegacyClient.send("gamma")
}
