TODO:

- ~bug: fix setPrices/raisePrices to override, not append~
- ~bug: 'step' is not properly recorded in VR/VP~
- ~parse & send raise-monument commands~
- ~render used markers~
- ~parse technology cards in player state~
- bug: pass size along to port renderers
- signup should be safer (validate email)

- Record game events that players didn't do (revenues)
- Fix winner declaration (tie breakers)
- display the supply
- redo player view
  - render players in turn order
  - show all of their stuff (technology cards, vr/vp round got, god description, specialists)
- display info on craftsman tiles (owner, at least)
- preview shouldn't cycle players

- login should be not stupid
  - Add 'auth_tokens' joined to user
  - update it when logging in
  - pass it back to user so they can store it locally without having to store password in plain text and can persist on a personal computer
  - pull from local storage on elm-init
  - place in local storage on login and signup
