type t =
  | Money of Money.t
  | Property of Property.t
  | Action of Action.t

let show = function
  | Money money -> Money.show money
  | Property property -> Property.show property
  | Action action -> Action.show action
