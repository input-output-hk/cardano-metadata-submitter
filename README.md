# Cardano Metadata Submitter

TODO: Get real build badge
[![Build status](https://badge.buildkite.com/e5b12d0fd507084fbdb1849da2de467f1de66b3e5c6d954554.svg)](https://buildkite.com/input-output-hk/iohk-nix)

A library and CLI for manipulating data intended for the [Goguen Metadata Registry](https://github.com/cardano-foundation/goguen-metadata-registry). See the link for more information about the Metadata Registry itself.

## Usage

### Building

TODO

### Accessing the registry

This tool is used in the context of editing a working copy of the [Goguen Metadata Registry](https://github.com/cardano-foundation/goguen-metadata-registry), and so in order to use it, you must first make a checkout of it, and
change your working directory to the `registry` subdirectory of it:

```bash
git checkout git@github.com:cardano-foundation/goguen-metadata-registry
cd goguen-metadata-registry/registry
```

### Creating a new entry or modifying an preexisting one:

Data is submitted to the Goguen Metadata Registry via pull request of a single file. The name of the file corresponds to the subject of the metadata entry. This CLI tool has been designed to fit into a git based workflow for creating and modifying entries.

Suppose we have a hash of an asset whose semantics we want to publish to the metadata repository. For example, this could be the hash of a smart contract we would like to document for the benefit of users, as Goguen scripts can only be invoked by providing the contents corresponding to their hash in the transactions that execute them.  To create a new metadata entry, while working in a checkout of the metadata repository, we can run the following command:

```
goguen-metadata-repository> cardano-metadata-submitter -i 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058
```

The result of running this query is a new file in the working directory, `3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058.json.draft`. Note the `.draft` extension which signals that we are still working on this piece of metadata. The contents of the file is as follows:

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058"
}
```

If you pass a subject that already has an entry to `-i`, then your draft will be a copy of the original.

### Adding a preimage

The most important piece of metadata for our script hash is, of course, the script itself. We can specify a preimage for our subject as such:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -H sha256 -p 'my-goguen-script'
```


```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "my-goguen-script"
        "hashFn": "sha256",
    }
}
```

### Adding multiple fields at once

We can specify multiple pieces of metadata at once. Here we will add a name and description for our script hash in one invocation:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -n "My Goguen Script" -d "A script I have registered on chain"
```


```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "sha256",
        "hashFn": "my-goguen-script"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
        ]
    }
}
```

### Adding attestation signatures to entries

Anyone can submit metadata to the repository. While the metadata is verified by humans before inclusion, it is desirable that there be a way to attest to the validity of the metadata with a signature. This can be done with an ED25519 key. You can use a signing key to attest to all fields present in an entry as follows:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -a attestation-key.prv
```

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "my-goguen-script",
        "hashFn": "sha256"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
            {
                "publicKey": "1bb5ddc6c2679bcffdf6e5f2f943d92922ce64df0ed20cee1b1aaef0cb6aba44",
                "signature": "233dc4b8a0632e9549ceb5c612595ca47a9223796100b9088c9bbbd5d9e79740a7e951d3cfd3ed68bea6a5d35f6934609b4fcc2046e3acaa9e4db3e753493509"
            }
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
            {
                "publicKey": "1bb5ddc6c2679bcffdf6e5f2f943d92922ce64df0ed20cee1b1aaef0cb6aba44",
                "signature": "a35cd71c6dbe429cdc79e4be8e2410121e9244e157258d34db694fa409e2d04663dfc51df70ee806d12dac24ad745e6b7e643896d61cda7c6dbf7cedbe36220b"
            }
        ]
    }
```

Note that if you modify fields that already have attestation signatures associated with them, those attestation signatures will be removed as they are no longer valid.

To only attest to one field, and not both, you can use flags. `-N` attests just the name, and `-D` attests just
the description. By default, both fields are attested if you provide an attestation key.

### Adding an ownership signature

Any number of signatures can be provided for each field in an entry, allowing multiple identities to attest to the validity of each metadata field for a given subject. The Goguen Metadata Registry also uses ED25519 signatures to verify ownership of a particular entry. Ownership is claimed by signing the entire entry, and only one ownership signature can be provided. You can sign for ownership as follows:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -o owner-key.prv
```

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "my-goguen-script",
        "hashFn": "sha256"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
            {
                "publicKey": "1bb5ddc6c2679bcffdf6e5f2f943d92922ce64df0ed20cee1b1aaef0cb6aba44",
                "signature": "233dc4b8a0632e9549ceb5c612595ca47a9223796100b9088c9bbbd5d9e79740a7e951d3cfd3ed68bea6a5d35f6934609b4fcc2046e3acaa9e4db3e753493509"
            }
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
            {
                "publicKey": "1bb5ddc6c2679bcffdf6e5f2f943d92922ce64df0ed20cee1b1aaef0cb6aba44",
                "signature": "a35cd71c6dbe429cdc79e4be8e2410121e9244e157258d34db694fa409e2d04663dfc51df70ee806d12dac24ad745e6b7e643896d61cda7c6dbf7cedbe36220b"
            }
        ]
    },
    "owner": {
        "publicKey": "17b34620988f919ad082f3f99bf8baa12d3646a1f03a55a790eaf38649eb0888",
        "signature": "0084fe036c60f74ee08ec5d1716a789803f8a2348ce86f41cbb483fd25a500eaea07db024719c5ad4c1fe09d8d13a1001744864860c13372b758c94661065800"
    }
}
```

As with attestation signatures, if you modify your entry at all after signing it for ownership, the ownership signature will be removed as it is invalid.

### Creating your PR branch

At this point we have added all of the metadata values we want to our entry, have attested to the validity of each field, and signed the entry with our ownership key. We are ready to finalize the entry and turn it into a pull request to the metadata repository:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -f
```

If you haven't been performing these operations in a checkout of your fork of the metadata repository, move your entry there now and you can perform the following steps to submit your entry:

```
git checkout -b "my-metadata-submission"
git add 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058.json
git commit -m "Example submission"
git push
```

At this point you can use Github to make your pull request. Remember to check CI and any comments made on your PR before its approval.

### Key Generation

As a convenience, cardano-metadata-submitter can generate an ED25519 keypair for you:

```
cardano-metadata-submitter -K attestation-key
```

This will create two files, `attestation-key.prv` and `attestation-key.pub`. Remember not to include these in your git commits, and store your private key with as much care as any other private key!
