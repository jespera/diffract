package com.example

class Repo {
    fun label(): String = "repo"

    override fun count(): Int = items.size

    fun total(x: Long): Long = x + offset

    val size: Int
        get() {
            return items.count()
        }
}
