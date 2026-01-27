// Order service
export class OrderService {
  private logger: Logger;
  private userService: UserService;

  constructor(logger: Logger, userService: UserService) {
    this.logger = logger;
    this.userService = userService;
  }

  async createOrder(userId: string, items: Item[]): Promise<Order> {
    this.logger.info("Creating order for user: " + userId);
    const user = await this.userService.getUser(userId);
    const order = new Order(user, items);
    await this.db.save(order);
    this.logger.info("Order created: " + order.id);
    return order;
  }

  async getOrder(orderId: string): Promise<Order> {
    this.logger.info("Fetching order: " + orderId);
    const order = await this.db.findById(orderId);
    if (!order) {
      this.logger.warn("Order not found: " + orderId);
      throw new Error("Order not found");
    }
    return order;
  }

  async cancelOrder(orderId: string): Promise<void> {
    this.logger.info("Cancelling order: " + orderId);
    const order = await this.getOrder(orderId);
    order.status = "cancelled";
    await this.db.save(order);
    this.logger.info("Order cancelled: " + orderId);
  }
}
