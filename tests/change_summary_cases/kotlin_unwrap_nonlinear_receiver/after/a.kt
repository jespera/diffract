object OrderFlow {
    fun eventListener(
        subscribers: Subscribers,
    ): Listener<Event, State> =
        SubscribersListener(subscribers, idFromUUID) { _, _ ->
            val message = if (isWorkflowCreation) {
                OrderMessage.OrderCreated(workflowId)
            } else {
                OrderMessage.OrderUpdated(workflowId)
            }
            broadcast(message)
        }
}
