const { execSync } = require('child_process');
const { readFileSync, writeFileSync } = require('fs');
const { mkdtemp } = require('fs/promises');
const os = require('os');
const path = require('path');
const { assert } = require('chai');

let cli, getDraft, withDraft, getFinal, writeTmpFile;


const alice = "19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0";
const bob = "04c24626761279476a9da9b9d851328cab93d92e7a8790852e42ff894b746f725a436f696e";

const policies =
    { [alice]: {
        "type": "all",
        "scripts": [
            { "type": "sig", "keyHash": "2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2" }
        ]
      }
    , [bob]: {
        "type": "any",
        "scripts": [
            { "type": "sig", "keyHash": "8dd214875687235fb55f9bc011aeded6fc53c989be261b46c20df4d4" },
            { "type": "sig", "keyHash": "a8a511c306eb0fd3e503116f594e9beb2274a3c178b172d3ed2e42cf" },
        ]
      }
    }

const policyFiles =
    { [alice]: alice+".policy.json"
    , [bob]: bob+".policy.json"
    }

const keys =
  { [alice]:
      [ `{ "type": "PaymentExtendedSigningKeyShelley_ed25519_bip32"
         , "description": "Payment Signing Key"
         , "cborHex": "5880807e4f00c6d7853c2c4e2da41d6047d1dded1ff4589ddcc8a6aadf71b13e6547ce73ced74b75236b3d9e143bb65104833ad3038b7739c82c4301d9ceb31ff7cdccce8b6a024f4c1d86bd44ed329885de5a6c8b73f45a407b66e030c13909f49a0c0250ec21e3c9c85c1d21fbf209a1b6610d3fa059b9ef48a68781e2d168b1ce"
         }`
      ]
  , [bob]:
      [ "addr_sk1pxpsyh9vac05ewq9eqey34zkvx06esedh6rlazdrja6epr7fmzfsdsyqag"
      , "0dc7a75e1337cb8f2f5902598f2dc1942a30d3c03333fc7d4834c3d2166718e2"
      ]
  }

const keyFiles =
    { [alice]: [alice+".0.sk"]
    , [bob]: [bob+".0.sk", bob+".1.sk"]
    }

describe("cardano-metadata-submitter", () => {
  describe("Minimal workflow", () => {
    before(fixture);

    // NOTE
    // Tests belows aren't 'independent' but ought to be ran in sequence.

    it("--init", () => {
      cli(alice, "--init");
      assert.equal(getDraft(alice).subject, alice);
    });

    it("Add required fields", () => {
      const name = `ギル`;
      const description = `The currency in all of the Final Fantasy games.`;
      const policy = `82008201818200581c2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2`

      cli(alice, "--name", name, "--description", description, "--policy", policy);

      const empty = { sequenceNumber: 0, signatures: [] };
      assert.deepEqual(getDraft(alice).name, { ...empty, value: name });
      assert.deepEqual(getDraft(alice).description, { ...empty, value: description });
      assert.deepEqual(getDraft(alice).policy, policy);
    });

    it("Validate after signing", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--finalize");
      assert.lengthOf(getFinal(alice).name.signatures, 1);
      assert.lengthOf(getFinal(alice).description.signatures, 1);
    });
  });

  describe("Draft editing", () => {
    before(fixture);
    beforeEach(() => cli(alice, "--init"));

    it("Reset content on --init", () => {
      cli(alice, "--name", "foo");
      assert.isNotNull(getDraft(alice).name);
      cli(alice, "--init");
      assert.isNull(getDraft(alice).name);
    });

    it("Edit property on successive calls", () => {
      let name = "SuperCoin"
      cli(alice, "--name", "foo");
      cli(alice, "--name", "bar");
      cli(alice, "--name", name);
      assert.equal(getDraft(alice).name.value, name);
      assert.equal(getDraft(alice).name.sequenceNumber, 2);
    });

    it("Remove signatures on edit to different value", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "--name", "foo")
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--name", "bar")
      assert.equal(getDraft(alice).name.value, "bar");
      assert.lengthOf(getDraft(alice).name.signatures, 0);
      assert.equal(getDraft(alice).name.sequenceNumber, 1);
    });

    it("Keep signatures on edit to same value", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "--name", "foo")
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--name", "foo")
      assert.equal(getDraft(alice).name.value, "foo");
      assert.lengthOf(getDraft(alice).name.signatures, 1);
      assert.equal(getDraft(alice).name.sequenceNumber, 0);
    });
  });

  describe("Signing", () => {
    before(fixture);
    beforeEach(() => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      writeTmpFile(policyFiles[alice], JSON.stringify(policies[alice]));
      cli(alice, "--init");
      cli(alice, "--name", "foo", "--description", "lorem ipsum");
      cli(alice, "--policy", policyFiles[alice]);

      writeTmpFile(keyFiles[bob][0], keys[bob][0]);
      writeTmpFile(keyFiles[bob][1], keys[bob][1]);
      writeTmpFile(policyFiles[bob], JSON.stringify(policies[bob]));
      cli(bob, "--init");
      cli(bob, "--name", "foo", "--description", "lorem ipsum");
      cli(bob, "--policy", policyFiles[bob]);
    });

    it("Alice can attest her metadata", () => {
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--finalize");
    });

    it("Bob can attest his metadata with key #0", () => {
      cli(bob, "-a", keyFiles[bob][0]);
      cli(bob, "--finalize");
    });

    it("Bob can attest his metadata with key #1", () => {
      cli(bob, "-a", keyFiles[bob][1]);
      cli(bob, "--finalize");
    });

    it("Bob can attest his metadata with keys #0 & #1", () => {
      cli(bob, "-a", keyFiles[bob][0]);
      cli(bob, "-a", keyFiles[bob][1]);
      cli(bob, "--finalize");
    });

    it("Can't finalize without attesting", () => {
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Can't finalize without attesting ALL properties", () => {
      cli(alice, "--attest-name", "-a", keyFiles[alice][0]);
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Keys from Bob don't attest for metadata of Alice", () => {
      cli(alice, "-a", keyFiles[bob][0]);
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Old signatures don't attest for new metadata", () => {
      cli(alice, "-a", keyFiles[alice][0]);
      const old = getDraft(alice);
      cli(alice, "--name", "bar");
      withDraft(alice, draft => {
        draft.name.signatures = old.name.signatures;
        return draft;
      });
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Signatures of an property don't work for another", () => {
      cli(alice, "--attest-name", "-a", keyFiles[alice][0]);
      withDraft(alice, draft => {
          draft.description.signatures = draft.name.signatures;
          return draft;
      });
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Bob can't use Alice's policy", () => {
      cli(bob, "--policy", policyFiles[alice]);
      cli(bob, "-a", keyFiles[bob][0]);
      try {
        cli(bob, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });
  });
});

function fixture(done) {
  mkdtemp(path.join(os.tmpdir(), "cardano-metadata-submitter"))
    .then(cwd => {
      cli = function cli(subject, ...args) {
        const prepared = args.map(arg => arg.startsWith("-") ? arg : `"${arg}"`);
        const opts = { cwd, stdio: 'ignore'  }
        // const opts = { cwd }
        return execSync(`cardano-metadata-submitter ${subject} ${prepared.join(" ")}`, opts);
      };

      getDraft = function getDraft(subject) {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json.draft")));
      }

      withDraft = function writeDraft(subject, fn) {
        const draft = getDraft(subject)
        writeFileSync(path.join(cwd, subject + ".json.draft"), JSON.stringify(fn(draft)));
      }

      getFinal = function getFinal(subject) {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json")));
      }

      writeTmpFile = function writeTmpFile(filename, buffer) {
        writeFileSync(path.join(cwd, filename), buffer);
      }
    })
    .then(done)
    .catch(console.fatal);
}
