package com.example.c

fun store(item: Notification<User>, db: Db): Unit = db.put(item)
