import { useState, useRef, useEffect } from 'react'
import * as React from 'react'
import * as cbor from 'cbor-web'
import './Goban.css'
import './App.css'
import {
    GobanCanvas,
    Goban,
} from "goban";

Goban.setCallbacks({
    getSelectedThemes: () => ({
        "board": "Anime",
        "removal-graphic": "square",
        "removal-scale": 1.0,
    }),
});

Goban.setCallbacks({
    getCoordinateDisplaySystem: () => "A1",
    getCDNReleaseBase: () => "",
});

function App (): JSX.Element {
  const [hydraHost, setHydraHost] = useState("ws://localhost:4001")
  const [godraHost, setGodraHost] = useState("http://localhost:4337")
  const [gameId, setGameId] = useState("")
  const [connected, setConnected] = useState(false);

  const container = useRef(null);
  let goban: Goban

  let resign = () => {
    console.log("Going to resign");
    fetch(`${godraHost}/resign/${gameId}`)
  };

  let connectToHydra = () => {
    goban = new GobanCanvas({
      interactive: true,
      mode: "puzzle",
      square_size: 60,
      board_div: container.current,
      width: 7,
      height: 7,
    });

    goban.on("cur_move", (m) => {
      console.log("Making move at", m.x, ",", m.y);
      fetch(`${godraHost}/move/${gameId}/${m.x}/${m.y}`)
        .then(() => console.log("Ok") )
        .catch(e => console.log("Error", e));
    });

    const hydraClient = new WebSocket(hydraHost);
    hydraClient.addEventListener("open", (e) => {
      setConnected(true);
    });

    hydraClient.addEventListener("message", e => {
      const msg = JSON.parse(e.data);
      switch (msg.tag) {
        case "TxValid":
          // TODO: Should only run on SnapshotConfirmed
          const cborHex       = msg.transaction.cborHex;
          const transaction   = cbor.decodeFirstSync(cborHex);
          const auxiliaryData = transaction[3]
          if (auxiliaryData !== undefined && auxiliaryData !== null) {
            const aux = auxiliaryData.value;
            const data = aux.get(0).get(715);

            if (data.gameId == gameId) {
              console.log("Found some data for this game: ", data);
              const { x, y } = data.action.contents;
              try {
                goban.engine.place(x, y);
              } catch (e) {
                // Note: This shouldn't ever happen in normal play.
                console.log("Failed placement: ", e);
              }
            };
          }
        default:
          // console.log("Irrelevant message", msg);
      }
    });
  };

  return (
    <>
      <h2> Godra </h2>
      { !connected && <div className="inputs">
        <div className="field">
          <label htmlFor="hydra host">Godra Host </label>
          <input type="text" name="godra-host"
            value={godraHost}
            onChange={(e) => setGodraHost(e.target.value)} />
        </div>
        <div className="field">
          <label htmlFor="hydra host">Hydra Host </label>
          <input type="text" name="hydra-host"
            value={hydraHost}
            onChange={(e) => setHydraHost(e.target.value)} />
        </div>
        <div className="field">
          <label htmlFor="game-id">Game ID</label>
          <input type="text" name="game-id"
            value={gameId}
            onChange={(e) => setGameId(e.target.value)} />
        </div>
        <div className="field">
          <button onClick={connectToHydra}>
            Connect
          </button>
        </div>
      </div>
      }

      <div className="Goban">
        <div ref={container}></div>
      </div>

      { connected &&
      <div className="inputs">
        <div className="field">
          <button onClick={resign}>
            Resign
          </button>
        </div>
      </div>
      }
    </>
  )
}

export default App


        // <div className="field">
        //   <label htmlFor="game-id">Your colour</label>
        //   <span>White</span>
        // </div>
