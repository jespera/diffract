export class BetaComponent {
  constructor(
    private readonly router: Router,
    private readonly store: Store,
    private readonly logger: ScopedLogger,
    private readonly http: HttpClient
  ) {}
}
