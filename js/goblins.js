// Copyright 2025 David Thompson <dave@spritely.institute>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// goblins.js is a JS wrapper around a Wasm build of Goblins.

class GoblinsObject {
  constructor(scm, goblins) {
    this.scm = scm;
    this.goblins = goblins;
  }
}

class Vat extends GoblinsObject {
  // Call 'f' within the context of a vat.
  async do(f) {
    return this.goblins.callWithVat(this, f);
  }

  // Like 'do' but for when a Goblins promise is the expected result
  // from 'f'.  The return value is a JS promise that can be
  // 'await'ed.
  doPromise(f) {
    return this.do(() => {
      const vow = f();
      return new Promise((resolve, reject) => {
        this.goblins.on(vow, (result) => {
          resolve(result);
        });
      });
    });
  }
}

class Actor extends GoblinsObject {
  // Synchronous call/return.
  sendSync(...args) {
    return this.goblins.sendSync(this, ...args);
  }

  // Asynchronous message send that returns a Goblins promise.
  send(...args) {
    return this.goblins.send(this, ...args);
  }

  // Fire and forget.
  sendOnly(...args) {
    this.goblins.sendOnly(this, ...args);
  }
}

class Goblins {
  static #wasmImports = {
    webSocket: {
      close(ws) { ws.close(); },
      new(url) {
        ws = new WebSocket(url);
        ws.binaryType = "arraybuffer";
        return ws;
      },
      send(ws, data) { ws.send(data); },
      setOnOpen(ws, f) { ws.onopen = (e) => { f(); }; },
      setOnMessage(ws, f) { ws.onmessage = (e) => { f(e.data); }; },
      setOnClose(ws, f) { ws.onclose = (e) => { f(e.code, e.reason); }; }
    },
    uint8Array: {
      new: (length) => new Uint8Array(length),
      fromArrayBuffer: (buffer) => new Uint8Array(buffer),
      length: (array) => array.length,
      ref: (array, index) => array[index],
      set: (array, index, value) => array[index] = value
    },
    crypto: {
      digest: (algorithm, data) => globalThis.crypto.subtle.digest(algorithm, data).then((arrBuf) => new Uint8Array(arrBuf)),
      randomValues(length) {
        const array = new Uint8Array(length);
        globalThis.globalThis.crypto.subtle.getRandomValues(array);
        return array;
      },
      generateEd25519KeyPair: () => globalThis.crypto.subtle.generateKey({ name: "Ed25519" }, true, ["sign", "verify"]),
      keyPairPrivateKey: (keyPair) => keyPair.privateKey,
      keyPairPublicKey: (keyPair) => keyPair.publicKey,
      exportKey: (key) => globalThis.crypto.subtle.exportKey("raw", key).then((arrBuf) => new Uint8Array(arrBuf)),
      importPublicKey: (key) => globalThis.crypto.subtle.importKey("raw", key, { name: "Ed25519" }, true, ["verify"]),
      signEd25519: (data, privateKey) => globalThis.crypto.subtle.sign({ name: "Ed25519" }, privateKey, data).then((arrBuf) => new Uint8Array(arrBuf)),
      verifyEd25519: (signature, data, publicKey) => globalThis.crypto.subtle.verify({ name: "Ed25519" }, publicKey, signature, data)
    }
  }
  #spawnVat;
  #callWithVat;
  #spawn;
  #sendSync;
  #send;
  #sendOnly;
  #on;

  static async init() {
    // Loading the Wasm binary returns a bunch of Scheme procedures
    // for the Goblins API that this class wraps.
    const [spawnVat, callWithVat, spawn, sendSync, send, sendOnly, on] =
      await Scheme.load_main("goblins.wasm", {
        user_imports: Goblins.#wasmImports
      });
    return new Goblins(spawnVat, callWithVat, spawn, sendSync, send, sendOnly, on);
  }

  constructor(spawnVat, callWithVat, spawn, sendSync, send, sendOnly, on) {
    this.#spawnVat = spawnVat;
    this.#callWithVat = callWithVat;
    this.#spawn = spawn;
    this.#sendSync = sendSync;
    this.#send = send;
    this.#sendOnly = sendOnly;
    this.#on = on;
  }

  spawnVat(name) {
    const [scm] = this.#spawnVat.call(name);
    return new Vat(scm, this);
  }

  async callWithVat(vat, f) {
    const results = await this.#callWithVat.call_async(vat.scm, f);
    return results.reflector.car(results);
  }

  spawn(constructor, ...args) {
    const [scm] = this.#spawn.call(constructor, ...args);
    return new Actor(scm, this);
  }

  sendSync(actor, ...args) {
    let [result] = this.#sendSync.call(actor.scm, ...args);
    return result;
  }

  send(actor, ...args) {
    let [result] = this.#send.call(actor.scm, ...args);
    // TODO: Should probably ensure this is a refr.
    return new Actor(result, this);
  }

  sendOnly(actor, ...args) {
    this.#sendOnly.call(actor.scm, ...args);
  }

  on(vow, fulfilled, rejected = false) {
    const [result] = this.#on.call(vow.scm, fulfilled, rejected);
  }
}
