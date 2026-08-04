package com.example.legacy

class Beta {
    fun ping(): String = LegacyClient.send("beta")
}
