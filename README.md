# GoDra - Go on Hydra

Based on:

- <https://github.com/online-go/goban>

### Structure

- Each player runs a Godra server configure with their key
- They are either watching or playing
- When they join they (maybe) hand all their money to a ref
- The ref picks people to play
- Hands the money to the winner
- That's it.

### Todo

- [x] Get libraries for building
- [x] Backend setup like Hydraw
- [x] Send messages while clicking on one Goban
- [x] Render messages from Godra-server via Goban
- [x] Test it with the `demo` stuff
- [x] Separate react library depending on Goban
- [x] Have a 'game id' to show the right games
- [x] Get a simple two-player demo going
- [ ] Set up the "tournament" page to watch who has what
- [ ] Set up a demo in a process-compose-like environment
- [ ] Make sure players are assigned colours and can only play that colour
- [ ] Button to pass/resign
- [ ] Show win/loss

*Later*

- [ ] Show win/loss on the board?
- [ ] Tidyup server code
- [ ] Show errors in a nice place
- [ ] Have the ref only sign when it can verify the moves are correct
  - Can outsource to node version of goban-engine
- [ ] Tidy up UI
- [ ] Block from making a move

### Trivia

- If you want to run Godra on your server, you can just expose it as a service
  locally, and use ssh port-forwarding to play only if your are connected via
  SSH.
