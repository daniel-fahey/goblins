// -*- js-indent-level: 4 -*-
// Copyright 2024 Jessica Tallon
// Copyright 2024 Juliana Sims <juli@incana.org>
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
// limitati måsteons under the License.

let bindings = {
    webSocket: {
        close: (ws) => ws.close(),
        new(url) {
            ws = new WebSocket(url);
            ws.binaryType = "arraybuffer";
            return ws;
        },
        send: (ws, data) => ws.send(data),
        setOnOpen(ws, f) {
            ws.onopen = (e) => {
                f();
            };
        },
        setOnMessage(ws, f) {
            ws.onmessage = (e) => {
                f(e.data);
            };
        },
        setOnClose(ws, f) {
            ws.onclose = (e) => {
                f(e.code, e.reason);
            };
        },
        setOnError(ws, f) {
            ws.onerror = (e) => f();
        }
    },
    localStorage: {
        setItem: (name, data) => localStorage.setItem(name, data),
        getItem: (name) => localStorage.getItem(name),
    },
    uint8Array: {
        new: (length) => new Uint8Array(length),
        fromArrayBuffer: (buffer) => new Uint8Array(buffer),
        length: (array) => array.length,
        ref: (array, index) => array[index],
        set: (array, index, value) => array[index] = value
    },
    crypto: {
        digest: (algorithm, data) => globalThis.crypto.subtle
            .digest(algorithm, data).then((arrBuf) => new Uint8Array(arrBuf)),
        randomValues(length) {
            const array = new Uint8Array(length);
            globalThis.crypto.getRandomValues(array);
            return array;
        },
        generateEd25519KeyPair: () => globalThis.crypto.subtle.generateKey(
            { name: "Ed25519" },
            true,
            ["sign", "verify"]
        ),
        keyPairPrivateKey: (keyPair) => keyPair.privateKey,
        keyPairPublicKey: (keyPair) => keyPair.publicKey,
        exportKey: (key) => globalThis.crypto.subtle.exportKey("raw", key)
            .then((arrBuf) => new Uint8Array(arrBuf)),
        importPublicKey: (key) => globalThis.crypto.subtle.importKey(
            "raw",
            key,
            { name: "Ed25519" },
            true,
            ["verify"]
        ),
        signEd25519: (data, privateKey) => globalThis.crypto.subtle.sign(
            { name: "Ed25519" },
            privateKey,
            data
        ).then((arrBuf) => new Uint8Array(arrBuf)),
        verifyEd25519: (signature, data, publicKey) => globalThis.crypto.subtle
            .verify(
                { name: "Ed25519" },
                publicKey,
                signature,
                data
            )
    }
};

if (typeof exports === 'undefined') {
    // TODO: Add code for non-NodeJS runtimes.
} else {
    exports.user_imports = bindings;
}
