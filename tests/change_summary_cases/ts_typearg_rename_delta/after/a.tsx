import { NewUserId } from "api/transformers";

type OwnerForm = {
  user: SelectState<"required", AppUser, NewUserId>;
};
