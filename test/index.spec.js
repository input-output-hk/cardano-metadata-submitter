const { execSync } = require('child_process');
const { readFileSync, writeFileSync } = require('fs');
const { mkdtemp } = require('fs/promises');
const os = require('os');
const path = require('path');
const { assert } = require('chai');

let cli, getDraft, getFinal, writeTmpFile;


const subjectA = "19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0";
const subjectB = "TODO";

const keys =
  { [subjectA]: `{
      "type": "PaymentExtendedSigningKeyShelley_ed25519_bip32",
      "description": "Payment Signing Key",
      "cborHex": "5880807e4f00c6d7853c2c4e2da41d6047d1dded1ff4589ddcc8a6aadf71b13e6547ce73ced74b75236b3d9e143bb65104833ad3038b7739c82c4301d9ceb31ff7cdccce8b6a024f4c1d86bd44ed329885de5a6c8b73f45a407b66e030c13909f49a0c0250ec21e3c9c85c1d21fbf209a1b6610d3fa059b9ef48a68781e2d168b1ce"
    }`
  , [subjectB]: ""
  }

describe("cardano-metadata-submitter", () => {
  describe("minimal workflow", () => {
    before(done => withTemporaryDirectory(subjectA, done));

    // NOTE
    // Tests belows aren't 'independent' but ought to be ran in sequence.

    it("--init", () => {
      cli("--init");
      assert.equal(getDraft().subject, subjectA);
    });

    it("add required fields", () => {
      const name = `ギル`;
      const description = `The currency in all of the Final Fantasy games.`;
      const policy = `82008201818200581c2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2`

      cli("--name", name, "--description", description, "--policy", policy);

      const empty = { sequenceNumber: 0, signatures: [] };
      assert.deepEqual(getDraft().name, { ...empty, value: name });
      assert.deepEqual(getDraft().description, { ...empty, value: description });
      assert.deepEqual(getDraft().policy, policy);
    });

    it("validate after signing", () => {
      try {
        cli("--finalize");
        assert.fail("should have thrown an error");
      } catch (e) {
        const keyfile = "secret.sk";
        writeTmpFile(keyfile, keys[subjectA]);
        cli("-a", keyfile);
        cli("--finalize");
        assert.lengthOf(getFinal().name.signatures, 1);
        assert.lengthOf(getFinal().description.signatures, 1);
      }
    });
  });

  describe("draft editing", () => {
    before(done => withTemporaryDirectory(subjectA, done));
    beforeEach(() => cli("--init"));

    it("reset content on --init", () => {
      cli("--name", "foo");
      assert.isNotNull(getDraft().name);
      cli("--init");
      assert.isNull(getDraft().name);
    });

    it("edit property on successive calls", () => {
      let name = "SuperCoin"
      cli("--name", "foo");
      cli("--name", "bar");
      cli("--name", name);
      assert.equal(getDraft().name.value, name);
      assert.equal(getDraft().name.sequenceNumber, 2);
    });

    it("remove signatures on edit to different value", () => {
      const keyfile = "secret.sk";
      writeTmpFile(keyfile, keys[subjectA]);
      cli("--name", "foo")
      cli("-a", keyfile);
      cli("--name", "bar")
      assert.equal(getDraft().name.value, "bar");
      assert.lengthOf(getDraft().name.signatures, 0);
      assert.equal(getDraft().name.sequenceNumber, 1);
    });

    it("keep signatures on edit to same value", () => {
      const keyfile = "secret.sk";
      writeTmpFile(keyfile, keys[subjectA]);
      cli("--name", "foo")
      cli("-a", keyfile);
      cli("--name", "foo")
      assert.equal(getDraft().name.value, "foo");
      assert.lengthOf(getDraft().name.signatures, 1);
      assert.equal(getDraft().name.sequenceNumber, 0);
    });

  });
});

function withTemporaryDirectory(subject, done) {
  mkdtemp(path.join(os.tmpdir(), "cardano-metadata-submitter"))
    .then(cwd => {
      cli = function cli(...args) {
        const prepared = args.map(arg => arg.startsWith("-") ? arg : `"${arg}"`);
        const opts = { cwd, stdio: 'ignore'  }
        return execSync(`cardano-metadata-submitter ${subject} ${prepared.join(" ")}`, opts);
      };

      getDraft = function getFile() {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json.draft")));
      }

      getFinal = function getFile() {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json")));
      }

      writeTmpFile = function writeTmpFile(filename, buffer) {
        writeFileSync(path.join(cwd, filename), buffer);
      }
    })
    .then(done)
    .catch(console.fatal);
}
