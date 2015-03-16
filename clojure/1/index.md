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

Fantastic.  Our project is all set up.  We are ready to _jack-in_ with Emacs and Cider and start coding.


## Cider Jack In and Experiment
Here is where we start to use the interactive nature of Clojure and Emacs in earnest.  With the _generator.clj_ file open in Emacs, type `M-x cider-jack-in`.
This will start a nREPL server for our project, so we can actively start to experiment with our code.  This early stage is a bit like playing with putty before sculpting.  It allows
us to quickly try out different approaches and get a feel for data constructs to use.  For example, type into `generator.clj` the line:

```clojure
(+ 1 1)
```

At this point, you can put your cursor at the end of the form and hit `C-x C-e` you will see the result `2` appear in the mini-buffer at the bottom of the screen.

Now, we are ready to experiment with Markov Chains.  The first thing we need is some small example to play with.  Consider the following text.

```
"And the Golden Grouse And the Pobble who"
```

To construct a Markov Chain, we need to transform this text into a chain of prefixes and suffixes.  In Markov chains, the length of the prefix can vary.  The larger the prefix, the more predictable the text becomes, while the smaller the prefix size, the moare random.  In this case, we are going to use a prefix size of 2.  We want to break up the original text into chunks of two words.  The suffix is the next word that comes after.

| Prefix        | Suffix
| ------------- |:-------------
| And the      | Golden
| the Golden   | Grouse
| Grouse And   | the
| And the      | Pobble
| the Pobble   | who
| Pobble who   | nil


This table becomes a guide for us in walking the chain to generate new text.  If we start at a random place in the table, we can generate some text by following some simple rules.


1. Choose a prefix to start.  Your result string starts as this prefix.
1. Take the suffix that goes with the prefix.  Add the suffix to your result string. Also, Add the last word of the prefix to the suffix, this is your new prefix.
1. Look up your new prefix in the table and continue until there is no suffix.
1. The result string is your generated text.

From our table, let's start with the prefix _the Pobble_.

1.  Our starting prefix is "the Pobble".  Our result string will be intialized to it.
1.  Look up the prefix in the table.  The suffix that goes with it is "who".  Add the suffix to the result string. The new prefix is the last word from the prefix and the suffix.  So the new prefix is "Pobble who".
1.  Lookup up the prefix in the table, the suffix is nil.  This means we have reached the end of the chain.  Our resulting text is "the Pobble who"

Things get interesting when there is more than one entry for a prefix.  Notice that _And the_ is in the table twice.  This means that there is a choice of what entry to use and what suffix.  We can randomly choose which one to use in our Markov Chain walk.  As a result, our text will be randomly generated. If start with the prefix _And the_ we have different  possibilities for the resulting text.  It could be

* And the Pobble who
* And the Golden Grouse And the Pobble who
* And the Golden Grouse And the Golden Grouse And the Pobble who
* And the Golden Grouse And the Golden Grouse And the Golden Grouse And the Pobble who 
* etc...

Since we could get into repeating chains, we should also put a terminating condition of the total length of our resulting text as well.

Now that we know the general idea of what we want to do, let's start small and start experimenting.

### Baby steps

First, let's take our example text and put it into code to play with in the REPL.

```clojure
(def example "And the Golden Grouse And the Pobble who")
;; -> #'markov-elear.core/example
```

Now, we are going to want to split up this text by spaces. This is a job for `clojure.string/split`.

```clojure
(def words (clojure.string/split example #" "))
words
;; -> ["And" "the" "Golden" "Grouse" "And" "the" "Pobble" "who"]
```


We also need to divide up these words in chunks of 3.  Clojure's `partition-all` will be perfect for this.  We are going to parition the word list in chunks of three.

```clojure
(def word-transitions (partition-all 3 1 words))
word-transitions
;; -> (("And" "the" "Golden")
;;     ("the" "Golden" "Grouse")
;;     ("Golden" "Grouse" "And")
;;     ("Grouse" "And" "the")
;;     ("And" "the" "Pobble")
;;     ("the" "Pobble" "who")
;;     ("Pobble" "who")
;;     ("who"))


This is nice, but we really need to get it into a word-chain format.  Ideally it would a map with the prefixes as the key and then have a set of suffixes to choose from.  So that the prefix of _And the_ would look like

```clojure
{["And" "the"]} #{"Pobble" "Golden"}
```

A map with the key being the vector of prefix words and the value being the set of suffixes.

We are clearly going to need to map through the list of word-transitions and build this up somehow.  Perhaps `merge-with` will help us out.

```clojure
(merge-with concat {:a [1]} {:a [3]})
;; -> {:a (1 3)}
```

`merge-with` will allow us to combine the prefixes with multiple suffixes in a map form, but we really want it in a set.  Time to experment some more.

```clojure
(merge-with clojure.set/union {:a #{1}} {:a #{2}})
;; -> {:a #{1 2}}
```

Yes, that will do nicely.  Let's try this out in a `reduce` over the `word-transitions`.

```clojure
(reduce (fn [r t] (merge-with clojure.set/union r
                               (let [[a b c] t]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions)
;; {["who" nil] #{},
;;  ["Pobble" "who"] #{},
;;  ["the" "Pobble"] #{"who"},
;;  ["Grouse" "And"] #{"the"},
;;  ["Golden" "Grouse"] #{"And"},
;;  ["the" "Golden"] #{"Grouse"},
;;  ["And" "the"] #{"Pobble" "Golden"}}
```

