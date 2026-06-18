import { NewUserId } from "api/transformers";

type ViewerForm = {
  owner: SelectState<"required", AppUser, NewUserId>;
};
