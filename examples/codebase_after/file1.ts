// User service
export class UserService {
  private logger: Logger;

  constructor(logger: Logger) {
    this.logger = logger;
  }

  async getUser(id: string): Promise<User> {
    this.logger.info("Fetching user: " + id);
    const user = await this.db.findById(id);
    if (!user) {
      this.logger.warn("User not found: " + id);
      throw new Error("User not found");
    }
    return user;
  }

  async updateUser(id: string, data: UserData): Promise<User> {
    this.logger.info("Updating user: " + id);
    const user = await this.getUser(id);
    Object.assign(user, data);
    await this.db.save(user);
    this.logger.info("User updated: " + id);
    return user;
  }

  async deleteUser(id: string): Promise<void> {
    this.logger.info("Deleting user: " + id);
    await this.db.delete(id);
  }
}
