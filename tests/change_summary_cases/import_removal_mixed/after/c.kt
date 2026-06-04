package com.example.app.notifications.di

import com.example.di.GlobalScope
import com.example.app.UnifiedResourceManager
import com.example.app.notifications.NotificationService
import com.example.app.notifications.Notifier
import com.example.app.notifications.PersistedNotificationService
import com.example.app.subscribers.SubscriberId
import com.example.app.subscribers.TransactionalSubscribers
import com.example.persistence.StorageBackend
import com.example.persistence.StoragePath
import dagger.Module
import dagger.Provides
import dagger.multibindings.Multibinds
import java.time.Clock

/**
 * Declares the set of notifiers. This allows it to be empty in case no bindings into the set
 * are actually added.
 */
@Module
interface DeclareNotifiersSetModule {
    @Multibinds
    fun notifiers(): Set<@JvmSuppressWildcards Notifier>
}

@Module(
    includes = [
        DeclareNotifiersSetModule::class,
        NotificationServerMessageModule::class,
    ],
)
class NotificationServiceModule {
    @Provides
    @GlobalScope
    fun provideNotificationService(
        storageBackend: StorageBackend,
        clock: Clock,
        notifiers: Set<@JvmSuppressWildcards Notifier>,
        subscribers: TransactionalSubscribers,
        unifiedResourceManager: UnifiedResourceManager,
    ): NotificationService = PersistedNotificationService.make(
        backend = storageBackend,
        path = StoragePath.of("notificationService"),
        notifiers = notifiers.toList(),
        clock = clock,
        subscribers = subscribers,
        resolveSubscriberId = { txn, it -> SubscriberId.UserId(it) },
        unifiedResourceManager = unifiedResourceManager,
    )
}

interface HasNotificationService {
    val notificationService: NotificationService
}
