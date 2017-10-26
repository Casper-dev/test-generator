# casper-test-generator

This repository contains a model (oracle) and a test generator for the [Casper token](https://github.com/Casper-dev/contract) written in [Haskell](https://www.haskell.org/).

The technique being used for tests is known as "model based testing".  The tester only needs to write scenarios that predictably transform the model's state on each step, and all the assertions will be automatically extracted from the model, which is itself an abstract representation of Casper token.

As for now, the model is very simple: only three ERC20 functions (think of them as model's state transitions) and only two events are  considered.

The model was written independently of the smart contract to reduce the probability of mistake transference.

## Installing

This package can be installed with [cabal](https://www.haskell.org/cabal/download.html)

```bash
cabal install
```

or examined interactively with

```bash
cabal repl
```

## Writing tests

Haskell's [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation) is utilized to form a DSL for test scenarios.

```haskell
import Casper

scenario = flatten $ do
  transfer me a 1
  approve me a 9
  transferFrom a me b 2
  approve me b 100000000000000000000000


code :: String
code = show $ scenarioToJS initialState scenario
```

The code will look like this:

```js
  it("\n      transfer me a 1\n      approve me a 9\n      transferFrom a me b 2\n      approve me b 100000000000000000000000", () => {
    return Casper.new().then(casper => {
      var utils = new TestUtils(web3, casper, [me,a,b]);
      return casper.transfer(a, 1, { from: me }).then(result => {
        utils.saveEvents(result);
        return utils.assertState([9999999,1,0], [[0,0,0],[0,0,0],[0,0,0]]).then(() => {
          return casper.approve(a, 9, { from: me }).then(result => {
            utils.saveEvents(result);
            return utils.assertState([9999999,1,0], [[0,9,0],[0,0,0],[0,0,0]]).then(() => {
              return casper.transferFrom(me, b, 2, { from: a }).then(result => {
                utils.saveEvents(result);
                return utils.assertState([9999997,1,2], [[0,7,0],[0,0,0],[0,0,0]]).then(() => {
                  return casper.approve(b, new BigNumber('100000000000000000000000'), { from: me }).then(result => {
                    utils.saveEvents(result);
                    return utils.assertState([9999997,1,2], [[0,7,new BigNumber('100000000000000000000000')],[0,0,0],[0,0,0]]).then(() => {
                      utils.assertLogs([['Transfer', me, a, 1],['Approval', me, a, 9],['Transfer', me, b, 2],['Approval', me, b, new BigNumber('100000000000000000000000')]]);
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
```