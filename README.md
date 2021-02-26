# Cardano Metadata Submitter

A library and CLI for manipulating data intended for the [Goguen Metadata Registry](https://github.com/cardano-foundation/goguen-metadata-registry). See the link for more information about the Metadata Registry itself.

## How to use

The instruction below supposes that users have already been through the process of creating a monetary script for Cardano and knows a bit about native assets on Cardano. If this doesn't a ring a bell, start off by reading through the [developer guide on Native Assets](https://developers.cardano.org/en/development-environments/native-tokens/native-tokens/). 

### Creating a new entry 

To create a new entry, you must first obtain your metadata subject. The subject is defined as the concatenation of the base16-encoded `policyId` and base16-encoded `assetName` of your asset. In case
your `assetName` is empty, then the `policyId` is your subject. From there, initialize a new submission using `--init` as follows:

```console
cardano-metadata-submitter --init 19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0
```

This creates a draft JSON file named after your subject. 

### Add required fields

Asset metadata have a set of required well-known properties. At minima, you'll therefore need to provide:

| Field         | Details                                         | Command               |
| ---           | ---                                             | ---                   |
| `name`        | at most 50 UTF-8 characters                     | `--name \| -n`        |
| `description` | at most 500 UTF-8 characters                    | `--description \| -d` |
| `policy`      | the exact script which hashes to the `policyId` | `--policy \| -p`      |

You can pass multiple commands at once to edit a draft submission. For example:

```console
cardano-metadata-submitter 19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0 \
  --name "ギル" \
  --description "The currency in all of the Final Fantasy games." \
  --policy "82008201818200581c2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2"
```

### Add optional fields

| Field    | Details                                                               | Command          |
| ---      | ---                                                                   | ---              |
| `ticker` | between 2 and 4 UTF-8 characters                                      | `--ticker \| -t` |
| `url`    | a valid https URI                                                     | `--url \| -h`    |
| `unit`   | a comma separated integer and a string of at most 30 UTF-8 characters | `--unit \| -u`   |
| `logo`   | a PNG image file                                                      | `--logo \| -l`   |

```console
cardano-metadata-submitter 19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0 \
  --ticker "GIL" \
  --url "https://finalfantasy.fandom.com/wiki/Gil" \
  --unit "2,cents" \
  --logo "icon.png"
```

### Sign metadata

Each metadata item must be signed with keys used to define your asset policy, such that, the resulting signatures validate the original monetary script. 
This allows for consumers to verify the authenticity of the metadata. So for example, if the policy is defined as a conjunction of n keys (all), then 
each item must be signed by each of those n keys. If the policy is defined as an n-of-m scheme, then signatures from only n of m keys are necessary. In 
case where the policy includes time-locking constraints, the metadata will be considered valid if the monetary script can be validated at the moment the
verification is done. It is therefore possible for a previously valid metadata to become invalid later in time.  

To attest all metadata at once, simple provide a signing key file (bech32, hexadecimal or cardano-cli's text envelope): 

> Alternatively, you can provide attestation for specific fields by using the upper-cased version of the command associated with that field, or by adding the `attest-` 
prefix to each long command (e.g. `-N` or `--attest-name` for `name`, `-T` or `--attest-ticker` for `ticker).

```console
cardano-metadata-submitter 19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0 -a policy.sk
```

The policy from this example is quite straightforward and simply requires all signatures from a single key. So a single attestation of that key for each metadata item is sufficient.

### Finalize submission

Finally, you can seal your submission using the `--finalize / -f` command. This will run some additional validations on your submission and check that it is 
considered valid. That is, it has to have sufficient attestations and must contain all the required fields. Once you have finalized a submission, you can still
update it alter on via the same process. Always think about using `--finalize` before submitting or re-submitting as a sanity check.

```console
cardano-metadata-submitter 19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0 --finalize
```

Your metadata is now ready to submit :tada:! 

## How to build

<details>
  <summary>Using Stack</summary>

```console
$ stack build
```
</details>

<details>
  <summary>Using Nix</summary>

`nix-build` will build all the library components, including test suite.

To get a shell which has the tool in scope you can use `nix-shell -A devops`.

#### Setting up a nix cache

For both building with `nix-build` and using `nix-shell`, it might take a very long
time if you do not have the Cardano binary cache set up. Adding the
IOHK binary cache to your Nix configuration will speed up builds a lot,
since many things will have been built already by our CI.

If you find you are building packages that are not defined in this
repository, or if the build seems to take a very long time then you may
not have this set up properly.

To set up the cache:

* On non-NixOS, edit /etc/nix/nix.conf and add the following lines:

```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

* On NixOS, set the following NixOS options:

```
nix = {
  binaryCaches          = [ "https://hydra.iohk.io" "https://iohk.cachix.org" ];
  binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" ];
};
```
</details>

## How to test

Pre-requisite: `cardano-metadata-submitter` must be available on your `$PATH`.

```
$ cd test
$ npm install
$ npm test
```
