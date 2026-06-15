package com.example.app

import com.example.workflows.WorkflowState

class ServiceB {
    fun describe(state: WorkflowState<Int>): String = state.toString()

    fun emit(n: Int) = Snapshot(
        id = n.toString(),
        state = build(::WorkflowState, n),
    )

    private val initial: WorkflowState<Int> = WorkflowState(value = 0)
}
