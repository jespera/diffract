package com.example

class Repo {
    fun label(): String {
        return "repo"
    }

    override fun count(): Int {
        return items.size
    }

    fun total(x: Long): Long {
        return x + offset
    }

    val size: Int
        get() {
            return items.count()
        }
}
