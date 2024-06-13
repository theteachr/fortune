include Fortune.Card

let show = function
  | Money money -> Money.show money
  | Property property -> Property.show_card property
  | Action action -> Action.show action
