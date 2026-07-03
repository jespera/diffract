export class BetaComponent {
  constructor(
    private readonly router: Router,
    private readonly store: Store,
    private readonly logger: Logger,
    private readonly http: HttpClient
  ) {}
}
