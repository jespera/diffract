package com.example.beta

import com.example.di.Module
import com.example.di.Provides

@Module
class BetaModule {
    private val defaultLimit = 42

    @Provides
    fun provideBeta(registry: Registry): BetaService = BetaService(registry)

    @Provides
    fun provideLimit(): Int = defaultLimit
}

@Module(includes = [BetaModule::class])
class BetaAggregatorModule {
    @Provides
    fun provideAggregate(beta: BetaService): Aggregate = Aggregate(beta)
}
