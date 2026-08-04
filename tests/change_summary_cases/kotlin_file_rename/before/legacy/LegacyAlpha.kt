package com.example.legacy

class Alpha {
    fun ping(): String = LegacyClient.send("alpha")
}
