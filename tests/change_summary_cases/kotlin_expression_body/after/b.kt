package com.example

class Service {
    private fun lookup(key: String): Value = cache[key]

    fun ready(): Boolean = started

    fun make(a: Int, b: Int): Result = Result(a, b)
}
