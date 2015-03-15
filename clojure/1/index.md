We are going to explore Clojure by creating a fun project together.  We are going to create a twitter bot that create its text based on a mashup of Edward Lear's poetry, ([The Project Gutenberg eBook, Nonsense Books, by Edward Lear](http://www.gutenberg.org/files/13650/13650-h/13650-h.htm) ), and a goodly selection of functional programming text taken from Wikipedia.  Why Edward Lear and Functional Programming?  Well first, because I really enjoy his poetry. My children grew up with me reading his _Nonsense Songs_ to them.  His whimsical writings, like his contemporary Lewis Carroll, appeal to me.  I also enjoy functional programming, which to be honest, has quite a few terms in it that sound odd.  I feel that Edward Lear, if he had known about functional programming, would have incorporated words like _monad_ and _endofunctor_ in his poetry.  This bot, will bring these two spheres together.

To get started,  we will cover getting started with a basic Clojure project and editor.  Then, we will build up our tweet generator with a [Markov Chain](http://en.wikipedia.org/wiki/Markov_chain).  Finally, we will deploy our code to [Heroku](https://www.heroku.com/) and hook it up to a twitter account, where it will live and tweet on its own.

Since this tutorial is geared to explain how I work in particular, we will start my essential ingredient to any coding project ... _tea_.  I brew myself a cup of _PG Tips_ tea with a splash of milk, then I sit down and fire up my trusty editor _Emacs_.

## Emacs is a lifestyle

Emacs is more than an editor, it is a lifestyle.  I also admit that the learning curve is steep.  I actually only know about 4% of Emacs. This is completely normal given that the learning curve for the editor looks like a squiggly curlicue.

![](http://mrozekma.com/editor-learning-curve.png)

Nevertheless, once I started using it for Clojure and experienced the interactive nature of the code and the REPL, (Read Eval Print Loop), I was hooked.  I use a customized version of [Emacs Starter Kit](https://github.com/technomancy/emacs-starter-kit). I also find the [Solarized Colorscheme](https://github.com/sellout/emacs-color-theme-solarized) a must for my eyes. For Clojure code, I use [Cider for Emacs](https://github.com/clojure-emacs/cider), which gives me the incredible interactive code experience that I was mentioning.

Now that we have our tea and emacs editor open, It is time to actually get our Clojure project created.  For this I use [Leiningen](http://leiningen.org/).

## Getting the basic project setup

Leiningen helps you create, manage, and automate your Clojure project.  If you don't already have Leiningen installed, follow the install [instructions](http://leiningen.org/) and download it.  We are going to call our project `markov-elear`, so to create a project we just type the `lein new` command at our prompt:

```
lein new markov-elear
```

This will create a basic project skeleton for us to work with.  The default src file that it creates is _src/markov_elear/core.clj_.  This is the first thing to change.  We want a more meaningful file name.  For our purposes, let's rename it to _src/markov_elear/generator.clj_. 

```
mv src/markov_elear/core.clj src/markov_elear/generator.clj
```

There is also a skeleton test file that is created in _test/markov_elear/core_test.clj_. We will want to do the same thing to it as well.

```
mv test/markov_elear/core_test.clj test/markov_elear/generator_test.js
```

Next, open up the _generator.clj_ file in Emacs.  It has been created with the Leiningen template, so there is code already there that looks like:

```clojure
(ns markov-elear.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
````

Since, we changed our file to be named _generator.clj_, we also need to change the namespace to match it.  Let's also get rid of the `foo` function.  It should now look like:

```clojure
(ns markov-elear.core)
```

Go ahead and open up the test file as well _test/markov_elear/generator_test.clj_.  It also has some sample code in it from the Leiningen template.  It looks like:

```clojure
(ns markov-elear.core-test
  (:require [clojure.test :refer :all]
            [foobar.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
```

Change the namespace in the test to match the filename as well.

```clojure
(ns markov-elear.generator-test
  (:require [clojure.test :refer :all]
            [foobar.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
```

At this point, we should now be able to run `lein test` from the command prompt and see our sample test fail.

```
lein test markov-eleaar.generator-test

lein test :only foobar.generator-test/a-test

FAIL in (a-test) (generator_test.clj:7)
FIXME, I fail.
expected: (= 0 1)
  actual: (not (= 0 1))

Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
Tests failed.

```

Fantastic.  Our project is all set up.  We are ready to _jack-in_ and start coding.
