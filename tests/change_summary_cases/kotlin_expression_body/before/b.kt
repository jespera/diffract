package com.example

class Service {
    private fun lookup(key: String): Value {
        return cache[key]
    }

    fun ready(): Boolean {
        return started
    }

    fun make(a: Int, b: Int): Result {
        return Result(a, b)
    }
}
