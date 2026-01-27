// User service
export class UserService {
  private logger: Logger;

  constructor(logger: Logger) {
    this.logger = logger;
  }

  async getUser(id: string): Promise<User> {
    console.log("Fetching user: " + id);
    const user = await this.db.findById(id);
    if (!user) {
      console.log("User not found: " + id);
      throw new Error("User not found");
    }
    return user;
  }

  async updateUser(id: string, data: UserData): Promise<User> {
    console.log("Updating user: " + id);
    const user = await this.getUser(id);
    Object.assign(user, data);
    await this.db.save(user);
    console.log("User updated: " + id);
    return user;
  }

  async deleteUser(id: string): Promise<void> {
    console.log("Deleting user: " + id);
    await this.db.delete(id);
  }
}
