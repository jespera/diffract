package com.example.alpha

import com.example.di.Module
import com.example.di.Provides

@Module
class AlphaModule {
    @Provides
    fun provideAlpha(dep: DepService, clock: Clock): AlphaService =
        AlphaService(dep, clock)
}
