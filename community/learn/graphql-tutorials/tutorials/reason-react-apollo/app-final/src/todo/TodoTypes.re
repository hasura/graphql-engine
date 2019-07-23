open UserTypes;

[@bs.deriving jsConverter]
type todo = {
  id: int,
  title: string,
  is_completed: bool,
  is_public: bool,
  user: option(user)
};
