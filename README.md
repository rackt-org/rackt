<p align="center">
  <img src="./logo.png" style="width: 300px" />
</p>

<h2 align="center"> An ultra small (~70 loc) React wrapper written in <a href="https://github.com/vishesh/racketscript">RacketScript</a></h2>


Rackt allows you to develop full-featured React web apps in RacketScript. You can use all React compatible libraries with it as well.

**Key featrures:**

- 🪶 **Ultra small**. Rackt is a pretty thin wrapper for React. Just consider you use React but with RacketScript.
- ⚡ **Super lightweight**. Compiled code takes only 6 Kb ungzipped.
- 🔌 **Easy to use API**. All transformations between JavaScript and RacketScript primitives happen under the hood. You can focus on writing code.
- ✨ **Modern**. It has first-class support of functional components and hooks.

Here you can see an example of a pretty simple Rackt-component:

```racket
(define (counter props ..)
    (define-values (counter set-counter) (use-state 0))

    (<el "div"
        (<el "button"
            #:props ($/obj [ className "button" ]
                   [ type "button" ]
                   [onClick (lambda (_) (set-counter (- counter 1)))])
            "- 1")

        (<el "span" #:props ($/obj [ className "counter" ]) counter)

        (<el "button"
            #:props ($/obj [ className "button" ]
                   [ type "button" ]
                   [onClick (lambda (_) (set-counter (+ counter 1)))])
            "+ 1")))

(render (<el counter) "root")
```

`<el` is just a simple wrapper for `React.createElement`. It has the same API but with a little bit different syntax:

```racket
(define create-element 
    (lambda (component #:props [props null] . children)

    ...
```

As you can see it takes `component`, optional `props` and any amount of `children`.

For other functions, you can see their implementation right [here](https://github.com/rackt-org/rackt/blob/master/main.rkt#L23-L48) and use React documentation for all of them.

You can find working examples [on the Rackt's main page](https://rackt-org.github.io/).

## Installation

It's unpublished in [the registry](https://pkgs.racket-lang.org/) for now, so to install the library you should clone the repo and install it manually:

```bash
git clone git@github.com:rackt-org/rackt.git
cd ./rackt
raco pkg install
```