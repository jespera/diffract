import { OldUserId, prettyCapability } from "domain/user";

type ViewerForm = {
  owner: SelectState<"required", AppUser, OldUserId>;
};
