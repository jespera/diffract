data class Slot(val holder: UserId)
fun reset() = w.update(holders = emptySet())
