package com.example.app

import com.example.workflows.WorkflowState

class ServiceA {
    fun current(): WorkflowState<String> = load()

    fun snapshot(id: String) = Snapshot(
        id = id,
        state = WorkflowState(
            value = lookup(id),
        ),
    )

    private fun load(): WorkflowState<String> = WorkflowState(value = "init")

    private fun lookup(id: String): String = id
}
