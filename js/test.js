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

window.addEventListener("load", async function() {
  // A constructor for a simple greeter actor that tracks how many
  // times it has been called.
  function greeter(become, ourName, timesCalled) {
    return (yourName) => {
      const newTimesCalled = timesCalled + 1;
      const msg = `Hello ${yourName}, my name is ${ourName}! I've been called ${newTimesCalled} times.`;
      return become(greeter(become, ourName, newTimesCalled), msg);
    };
  }

  // Load Goblins Wasm module.
  const goblins = await Goblins.init();
  // Spawn a fresh vat.
  const vat = goblins.spawnVat("test");
  // Spawn Alice within the vat.
  const alice = await vat.do(() => goblins.spawn(greeter, "Alice", 0));
  // Send a message to Alice asynchronously and print the result.
  console.log(await vat.doPromise(() => alice.send("Bob")));
});
