package com.example.app.accounts.di

import com.example.di.GlobalScope
import com.example.di.PlatformIdentity
import com.example.app.Authentication
import com.example.app.Capability
import com.example.app.AppUserId
import com.example.app.PermissionContextWithReadTransaction
import com.example.app.RestrictedDataView
import com.example.app.Scope
import com.example.app.accounts.CashDeposit
import com.example.app.accounts.CashDepositAnswerJsonCodec
import com.example.app.accounts.CashDepositEventJsonCodec
import com.example.app.accounts.ProvidedCashDepositService
import com.example.app.accounts.ResourceLedger
import com.example.app.accounts.unifiedResources.di.CashDepositRequestUnifiedResourceModule
import com.example.app.auditroll.AuditEvent
import com.example.app.auditroll.AuditRoll
import com.example.app.id.CashDepositRequestId
import com.example.app.identity.IdentityManager
import com.example.app.isCapable
import com.example.app.notifications.NotificationService
import com.example.app.subscribers.TransactionalSubscribers
import com.example.app.workflows.InternalAppWorkflowState
import com.example.app.workflows.WorkflowServiceImpl
import com.example.app.workflows.WorkflowServiceManager
import com.example.app.workflows.di.BindOptionalWorkflowServiceManagerModule
import com.example.identity.Identity
import com.example.persistence.StorageBackend
import com.example.persistence.StoragePath
import com.example.serialization.dsl.typed.TypedJsonSum
import com.example.workflows.SimpleOf
import dagger.Module
import dagger.Provides
import dagger.multibindings.IntoSet
import java.time.Clock

typealias DepositRequest =
    InternalAppWorkflowState<CashDeposit.State, CashDeposit.Event, SimpleOf<CashDeposit.Event>>

@Module(
    includes = [
        BindOptionalWorkflowServiceManagerModule::class,
        AccountsServerMessageModule::class,
        CashServerMessageModule::class,
        CashDepositRequestUnifiedResourceModule::class,
    ],
)
class CashDepositServiceModule {
    @Provides
    @GlobalScope
    fun provideCashDepositService(
        identityManager: IdentityManager,
        resourceLedger: ResourceLedger,
        clock: Clock,
        storageBackend: StorageBackend,
        @PlatformIdentity platformIdentity: Identity,
        manager: WorkflowServiceManager,
        auditRoll: AuditRoll,
        subscribers: TransactionalSubscribers,
        notificationService: NotificationService<AppUserId>,
    ): ProvidedCashDepositService = WorkflowServiceImpl.make(
        name = "Cash deposit",
        clock = clock,
        backend = storageBackend,
        path = StoragePath.of("cashDepositWorkflow"),
        eventCodec = CashDepositEventJsonCodec,
        answerCodec = CashDepositAnswerJsonCodec,
        workflowFactory = { _, _ ->
            CashDeposit.Workflow(
                CashDeposit.Actions(
                    identityManager = identityManager,
                    resourceLedger = resourceLedger,
                ),
            )
        },
        workflowIdFromUUID = ::CashDepositRequestId,
        manager = manager,
        workflowEventListener = CashDeposit.eventListener(
            platformIdentity = platformIdentity,
            identityManager = identityManager,
            auditRoll = auditRoll,
            subscribers = subscribers,
            notificationService = notificationService,
        ),
        getObservation = { it.observation },
        restrictedDataView = object : RestrictedDataView<DepositRequest> {
            override fun PermissionContextWithReadTransaction<DepositRequest>.isViewableBy(
                authentication: Authentication,
            ): Boolean =
                authentication.isCapable(Capability.CanManageCashDepositRequests(Scope.Read)) ||
                    identity == entity.state.input.identity
        },
    ).let(::ProvidedCashDepositService)

    @Provides
    @IntoSet
    fun provideCashDepositAuditCodec(): TypedJsonSum<AuditEvent> = CashDeposit.Audit.codec
}

interface HasCashDepositService {
    val cashDepositService: ProvidedCashDepositService
}
