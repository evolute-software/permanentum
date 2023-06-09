# Permanentum

> decentralized, guaranteed storage of Cardano data

This is the repository of the open source implementation of permanentum. In time the whole project will be available here.

homepage: [permanentum.io](https://permanentum.io)

## Documentation

[DOCS.md](./DOCS.md)

## Running

### 1. Spin up the services

To run a local instance just clone this repo and run:
```
cp example.env .env
# OPT: edit .env to your liking
docker compose up
```

NOTE: Do not try to pull the images, the locations in the `image:` arguments
      point to an internal image registry of evolute.software

This will start a number of services, amongst others 

#### Lotus

No UI
and

#### IPFS

web ui can be found at `http://${TUN0}:5001/webui`

### 2. Generate Keys and fund from faucets

#### Filecoin

You can generate a fil wallet by starting a bash shell on the lotus service
and using the lotus cli:

```
docker compose exec lotus bash

# Inside the container run
lotus sync wait # This will wait until the node is syncronized. If it fails
                # the most probable cause is that the node is still importing
                # the Snapshot

lotus wallet new # Creates a new wallet and prints our its address
```

Now that you have a wallet, head over to the 
[calibnet faucet](https://faucet.calibration.fildev.network/) 
and send yourself some funds. Run `lotus wallet list` to see whether the funds
arrived.



### Cardano

> tbd
