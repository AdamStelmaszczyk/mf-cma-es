# _Covariance Matrix Adaptation Evolution Strategy without a matrix_

Jarosław Arabas, Adam Stelmaszczyk, Eryk Warchulski, Dariusz Jagodziński, Rafał Biedrzycki  
Institute of Computer Science, Warsaw University of Technology

## Installation

Install podman (https://podman.io/docs/installation), on *nix:
```
apt install podman
```
Navigate to the directory containing `Dockerfile` and build a `cmaes` image (it takes about a minute):
```
podman build -t cmaes .
```

## Running

Start a container and step into it with bash:

```
podman run -it --entrypoint bash cmaes
```


Start the chosen R script (`experiments/benchmarks/run.R`, `experiments/convergence/run.R` or `experiments/experimentum-crucis/run.R`):

```
root@b97bfec4d9da:/usr/src/app# Rscript experiments/experimentum-crucis/run.R
```

`experimentum-crucis` takes about a minute to run.

The data will appear in the `experiments/experimentum-crucis/data` directory. 

To view it you can use `ls`, `more`, `head` etc.

```
root@7f872a2b5723:/usr/src/app# ls -l experiments/experimentum-crucis/data/
total 4260
-rw-r--r-- 1 root root 2175659 Jan 29 17:36 experimentum-crucis-nm-cma-es-vectorized-w-linear-30-d.csv
-rw-r--r-- 1 root root 2172528 Jan 29 17:35 experimentum-crucis-vanilla-cma-es-30-d.csv
```

Or you can download files from the container to your host - while having the container running, execute this in a second command line tab on your host:
```
podman cp 7f872a2b5723:experiments/experimentum-crucis/data .
```

## Clean up

List your images:
```
podman images
```

Remove one image:
```
podman rmi -f IMAGE_ID
```

Remove all images:
```
podman rmi -f $(podman images -q)
```

List all containers:
```
podman ps -a
```

Remove ~everything (images, containers...):
```
podman system prune -a
```
