package com.example.app.auction.di

import com.example.di.GlobalScope
import com.example.di.PlatformIdentity
import com.example.di.ReportingAdminAddress
import com.example.di.ReportingBaseCurrency
import com.example.di.SupportedCharge
import com.example.app.Authentication
import com.example.app.Capability
import com.example.app.Currency
import com.example.app.AppUserId
import com.example.app.PermissionContextWithReadTransaction
import com.example.app.RestrictedDataView
import com.example.app.Scope
import com.example.app.accounts.ResourceLedger
import com.example.app.auction.Auction
import com.example.app.auction.Auction.Event
import com.example.app.auction.Auction.State
import com.example.app.auction.AuctionAnswerJsonCodec
import com.example.app.auction.AuctionEventDesc
import com.example.app.auction.AuctionEventJsonCodec
import com.example.app.auction.CARBON_MARKET_AUCTION_FEE
import com.example.app.auction.INSTRUMENT_MARKET_AUCTION_FEE
import com.example.app.auction.ProvidedAuctionService
import com.example.app.auction.REC_MARKET_AUCTION_FEE
import com.example.app.auction.UNTRACKED_PRODUCT_MARKET_AUCTION_FEE
import com.example.app.auction.mkAuctionActions
import com.example.app.auction.mkAuctionReportGenerator
import com.example.app.auditroll.AuditEvent
import com.example.app.auditroll.AuditRoll
import com.example.app.documents.generator.DocumentGenerator
import com.example.app.externalCredit.ExternalCreditReferenceDataService
import com.example.app.fees.FeeInputUnit
import com.example.app.fees.service.ChargeMetadata
import com.example.app.fees.service.FeesService
import com.example.app.fees.service.FunctionType
import com.example.app.id.AuctionRequestId
import com.example.app.identity.IdentityManager
import com.example.app.isCapable
import com.example.app.issuance.Issuance
import com.example.app.legalentities.Address
import com.example.app.notifications.EmailNotifier
import com.example.app.notifications.EmailRenderer
import com.example.app.notifications.NotificationService
import com.example.app.reports.ReportDocuments
import com.example.app.reports.di.TradeReportDocumentsModule
import com.example.app.subscribers.TransactionalSubscribers
import com.example.app.workflows.InternalAppWorkflowState
import com.example.app.workflows.WorkflowServiceImpl
import com.example.app.workflows.WorkflowServiceManager
import com.example.app.workflows.di.BindOptionalWorkflowServiceManagerModule
import com.example.identity.Identity
import com.example.persistence.StorageBackend
import com.example.persistence.StoragePath
import com.example.serialization.dsl.typed.TypedJsonSum
import com.example.workflows.AutomationService
import com.example.workflows.JobScheduler
import dagger.Module
import dagger.Provides
import dagger.multibindings.IntoMap
import dagger.multibindings.IntoSet
import dagger.multibindings.StringKey
import java.time.Clock
import java.util.Optional
import kotlin.jvm.optionals.getOrNull

@Module(
    includes = [
        AuctionServerMessageModule::class,
        BindOptionalWorkflowServiceManagerModule::class,
        TradeReportDocumentsModule::class,
    ],
)
class AuctionServiceModule {
    @Provides
    @GlobalScope
    fun provideAuctionService(
        clock: Clock,
        storageBackend: StorageBackend,
        @PlatformIdentity platformIdentity: Identity,
        documentGenerator: DocumentGenerator,
        reportDocuments: ReportDocuments,
        identityManager: IdentityManager,
        feesService: FeesService,
        externalCreditReferenceDataService: Optional<ExternalCreditReferenceDataService>,
        issuanceService: Optional<Issuance>,
        resourceLedger: ResourceLedger,
        manager: WorkflowServiceManager,
        auditRoll: AuditRoll,
        subscribers: TransactionalSubscribers,
        notificationService: NotificationService<AppUserId>,
        jobScheduler: JobScheduler,
        @ReportingAdminAddress adminAddress: Address,
        @ReportingBaseCurrency baseCurrency: Currency,
        emailRenderer: EmailRenderer,
        emailNotifier: Optional<EmailNotifier<AppUserId>>,
    ): ProvidedAuctionService = WorkflowServiceImpl.make(
        name = "Auction service",
        clock = clock,
        backend = storageBackend,
        path = StoragePath.of("auctionWorkflow"),
        eventCodec = AuctionEventJsonCodec,
        answerCodec = AuctionAnswerJsonCodec,
        workScheduler = AutomationService.make(
            storageBackend,
            StoragePath.of("auctionAutomation"),
            jobScheduler,
        ),
        workflowFactory = { service, id ->
            val docGen = mkAuctionReportGenerator(
                platformIdentity = platformIdentity,
                documentGenerator = documentGenerator,
                reportDocuments = reportDocuments,
                adminAddress = adminAddress,
                identityManager = identityManager,
                auctionService = service,
                issuanceService = issuanceService,
                externalCreditReferenceDataService = externalCreditReferenceDataService,
                clock = clock,
                emailNotifier = emailNotifier.getOrNull(),
                emailRenderer = emailRenderer,
            )
            val actions = mkAuctionActions(
                identityManager = identityManager,
                externalCreditReferenceDataService = externalCreditReferenceDataService,
                issuanceService = issuanceService,
                fees = feesService,
                resourceLedger = resourceLedger,
                auctionService = service,
                platformIdentity = platformIdentity,
                auctionDocGen = docGen,
            )
            Auction.Workflow(
                workflowId = id,
                channel = service.getSystemEventChannel(id),
                actions = actions,
                baseCurrency = baseCurrency,
            )
        },
        workflowIdFromUUID = ::AuctionRequestId,
        manager = manager,
        workflowEventListener = Auction.eventListener(
            auditRoll = auditRoll,
            subscribers = subscribers,
            notificationService,
            identityManager,
            externalCreditReferenceDataService,
            issuanceService,
            platformIdentity = platformIdentity,
        ),
        getObservation = { it.observation },
        restrictedDataView = object :
            RestrictedDataView<InternalAppWorkflowState<State, Event, AuctionEventDesc>> {
            override fun PermissionContextWithReadTransaction<InternalAppWorkflowState<State, Event, AuctionEventDesc>>.isViewableBy(
                authentication: Authentication,
            ): Boolean {
                val isAdmin = authentication.isCapable(Capability.CanManageAuctions(scope = Scope.Update))
                val isInvitedSupplier = entity.state.suppliersMap.any {
                    it.value.first.supplier == identity
                }
                val isInvitedBidder = entity.state.permittedBidders.contains(identity)
                val isPublished = entity.state.published
                return isPublished || isAdmin || isInvitedSupplier || isInvitedBidder
            }
        },
    ).let(::ProvidedAuctionService)

    @Provides
    @IntoSet
    fun provideAuctionAuditCodec(): TypedJsonSum<AuditEvent> = Auction.Audit.codec

    @Suppress("ktlint:standard:function-naming")
    @Provides
    @GlobalScope
    @IntoMap
    @StringKey(CARBON_MARKET_AUCTION_FEE)
    @SupportedCharge
    fun provideCARBON_MARKET_AUCTION_FEE(): ChargeMetadata =
        ChargeMetadata(
            inputUnit = FeeInputUnit.CURRENCY_SUBUNIT,
            functionType = FunctionType.RATE_ONLY,
            feeInputStepSize = null,
            isProfitStealing = true,
        )

    @Suppress("ktlint:standard:function-naming")
    @Provides
    @GlobalScope
    @IntoMap
    @StringKey(REC_MARKET_AUCTION_FEE)
    @SupportedCharge
    fun provideREC_MARKET_AUCTION_FEE(): ChargeMetadata =
        ChargeMetadata(
            inputUnit = FeeInputUnit.CURRENCY_SUBUNIT,
            functionType = FunctionType.RATE_ONLY,
            feeInputStepSize = null,
            isProfitStealing = true,
        )

    @Suppress("ktlint:standard:function-naming")
    @Provides
    @GlobalScope
    @IntoMap
    @StringKey(INSTRUMENT_MARKET_AUCTION_FEE)
    @SupportedCharge
    fun provideINSTRUMENT_MARKET_AUCTION_FEE(): ChargeMetadata =
        ChargeMetadata(
            inputUnit = FeeInputUnit.CURRENCY_SUBUNIT,
            functionType = FunctionType.RATE_ONLY,
            feeInputStepSize = null,
            isProfitStealing = true,
        )

    @Suppress("ktlint:standard:function-naming")
    @Provides
    @GlobalScope
    @IntoMap
    @StringKey(UNTRACKED_PRODUCT_MARKET_AUCTION_FEE)
    @SupportedCharge
    fun provideUNTRACKED_PRODUCT_MARKET_AUCTION_FEE(): ChargeMetadata =
        ChargeMetadata(
            inputUnit = FeeInputUnit.CURRENCY_SUBUNIT,
            functionType = FunctionType.RATE_ONLY,
            feeInputStepSize = null,
            isProfitStealing = true,
        )
}

interface HasAuctionService {
    val auctionService: ProvidedAuctionService
}
