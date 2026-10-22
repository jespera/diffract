object InvoiceFlow {
    fun eventListener(
        subscribers: Subscribers,
    ): Listener<Event, State> =
        SubscribersListener(subscribers, idFromUUID) { _, _ ->
            val message = if (isWorkflowCreation) {
                InvoiceMessage.InvoiceCreated(workflowId)
            } else {
                InvoiceMessage.InvoiceUpdated(workflowId)
            }
            broadcast(message)
        }
}
