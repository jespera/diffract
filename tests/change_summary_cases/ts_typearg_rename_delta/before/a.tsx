import { OldUserId, prettyCapability } from "domain/user";

type OwnerForm = {
  user: SelectState<"required", AppUser, OldUserId>;
};
