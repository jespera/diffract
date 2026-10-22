object ShipmentFlow {
    fun eventListener(
        subscribers: Subscribers,
    ): Listener<Event, State> =
        SubscribersListener(subscribers, idFromUUID) { _, _ ->
            val message = if (isWorkflowCreation) {
                ShipmentMessage.ShipmentCreated(workflowId)
            } else {
                ShipmentMessage.ShipmentUpdated(workflowId)
            }
            broadcast(message)
        }
}
