package com.example.gamma

import com.example.di.Module
import com.example.di.Provides

@Module
class GammaModule {
    @Provides
    fun provideGamma(dep: DepService, clock: Clock): GammaService =
        GammaService(dep, clock, retries = 3)
}

@Module
class KeeperModule {
    companion object {
        private val log = logger { }
    }

    @Provides
    fun provideKeeper(store: Store): KeeperService {
        log.info("creating keeper")
        return KeeperService(store)
    }
}
