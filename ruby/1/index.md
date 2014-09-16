## Intro

Hi there! I'm Steve. *waves*. Let's build a Ruby gem together!

There are a few key steps to building a gem. Here's what we need to do:

1. Figure out what our gem needs to do.
2. Install git
3. Install Ruby
4. Generate the skeleton
5. Write some tests
6. Implement the code
7. Write some docs
8. Push to GitHub
9. Push to RubyGems
10. Tweet about it

Ten easy steps. Let's do this!

Oh, one more comment before I start: I use `vim` to do all of my editing.  Many
people also use Textmate on Mac OS X, and Sublime is becoming increasingly
popular. But I like `vim`. I don't use any plugins, either. I do like
[cntrl-p](https://github.com/kien/ctrlp.vim).  In general, I keep my tooling
very vanilla; it helps when I pair, when I teach, and probably to tell you the
truth I'm probably just lazy.

Anyway. Ten steps!

## Figure out what our gem needs to do.

The most important part! Every gem needs some kind of raison d'être. Any time I
have some Ruby code I think someone else might use, I try to extract it into a
gem. This means I currently have push access to 38 gems, and I am a committer
on many more. Oh well, everyone has to have a hobby...

In this case, we want to make a gem so that I can show you how I make a gem.
So we've got a name, which is often the hardest part of making the damn thing.
We'll call it "`how_i_start`". Even with just the name, we already have
something to talk about: naming conventions. Ruby gems should be named with all
lower case letters, and no punctuation other than `_` and `-`. Use `_` for
separating words, and `-` to indicate an extension to an existing gem.

Not every single gem follows these conventions, especially if they are quite
old. For example, [`activerecord`](https://rubygems.org/gems/activerecord)
should really be `active_record`. If you're starting a new gem, don't
contribute to this confusion! You can find a slightly more full description of
these naming guidelines [on the RubyGems
Guides](http://guides.rubygems.org/name-your-gem/).

So, we've got a name, but what should it do? Let's make it do something very
simple: we'll include a simple executable that prints out a link to this
article. That'll be a very small amount of behavior, and also be relevant.

I've already written and released a gem with this name, so if you want to push
your own version of the RubyGem, you'll have to change it to something else.
Sorry about that! Maybe try the extension convention for names, and call yours
`how_i_start-jonathan`. Of course, only do that if your name is Jonathan, or
he'll be really disappointed when he reads this article. I've got your back,
Jonathan!

## Install git

Rubyists use `git` to manage versions. If you don't like it, well... sorry.
You're gonna have a bad time. Everything in Ruby world assumes `git`.

Since I'm on a Debian-based Linux, I just type this:

```bash
$ apt-get install git
```

Done. This is probably the easiest step in the whole thing. Well, maybe not.
But one command is pretty easy.

You can't skip this step. Later, our tools will assume that we have `git`
installed. It's a bit fascist, I'll admit, but at least the trains run on time.
:[

## Install Ruby

We can't make a gem without Ruby! It's also essential to use some sort of tool
to switch between different Ruby versions, as well. When a new version of Ruby
comes out, we want all those new goodies.

There are a bunch of options here, but I prefer minimalism. I use
[ruby-build](https://github.com/sstephenson/ruby-build) to install Ruby, and
[chruby](https://github.com/postmodern/chruby) ("chuh ruby") to **ch**ange
between different **ruby**s.

Oh, I should also mention that I use Linux almost exclusively. These
instructions will also work almost unchanged on an Apple computer, if you
happen to be like many Rubyists. If you're on Windows, I highly recommend
[RubyInstaller](http://rubyinstaller.org/). I don't use a version switcher when
I'm on Windows.

Let's install `ruby-build` so we can build and install a Ruby. I wonder if I
could have fit more 'install' and 'build' and 'Ruby' into that sentence. Anyway,
it's simple:

```bash
git clone https://github.com/sstephenson/ruby-build.git
cd ruby-build
./install.sh
```

You may need a `sudo` if you don't have permissions to install to `/usr/local`.

On my machine, `/usr/local/bin` is already in my `$PATH`, so it just works:

```bash
$ ruby-build --definitions
```

This prints out all the different versions of Ruby that `ruby-build` knows
how to build:

```bash
$ ruby-build --definitions | wc -l
109
```

Whoah, 109 versions so far!

```bash
$ ruby-build --definitions | grep "2\.1"
2.1.0
2.1.0-dev
2.1.0-preview1
2.1.0-preview2
2.1.0-rc1
2.1.1
rbx-2.1.0
rbx-2.1.1
rbx-2.2.1
```

Ruby 2.1 is the latest version of Ruby that's out right now, and there's even
a bunch of them! We want `2.1.1`. Let's build it:

```bash
ruby-build 2.1.1 ~/.rubies/2.1.1
```

You tell `ruby-build` which version of Ruby to build, and where you want to put
it.  `~/.rubies` is one of the places that `chruby` looks by default, and I
like that each user on the machine can have their own Rubies. I mean, I'm the
only person (other than the NSA, probably) that is using my laptop, but still,
keeping it all local to your regular user is nice.

That should run for a while, and then you have a Ruby installed! Next, `chruby`.
`chruby` is sweet because you're able to type:

```bash
$ chruby 2.0.0
$ ruby -v
ruby 2.0.0p0 (2014-01-01) [x86_64-linux]
$ chruby 2.1.1
ruby 2.1.1p76 (2014-02-24 revision 45161) [x86_64-linux]
```

Nice and easy. It's great for being able to test your gem against multiple Ruby
versions, or when a new Ruby comes out that you'd like to use.

Let's install it:

```bash
$ wget -O chruby-0.3.8.tar.gz https://github.com/postmodern/chruby/archive/v0.3.8.tar.gz
$ tar -xzvf chruby-0.3.8.tar.gz
$ cd chruby-0.3.8/
$ sudo make install
```

You then have to do one more thing: add a line to your shell's profile. I use
bash, so mine is `~/.bashrc`. If you're cooler than me, you'll probably edit
`~/.zshrc`. Either way, add this line:

```bash
source /usr/local/share/chruby/chruby.sh
```

This loads up `chruby`, which is basically just a shell script. I happen to
like automatically switching to a particular Ruby, so I also add

```bash
chruby 2.1.1
```

This means that I'll always have 2.1.1 right at my fingertips. Of course, given
that it's a shell script, this is also how you use `chruby`. Just type the
version after the name, and you're good to go. Easy peasy.

That's it for tooling! We're all good to go. Now, let's dig in to the
gem-building specific stuff.

## Generate the skeleton

Turns out that we're barely gonna even need to do any setup, as there's a tool
that does it for us. Ruby is super-huge on convention, so that means we have
pretty awesome tools. They'd be even better with static types, but what'cha
gonna do?

To do the generating, we need to install [Bundler](http://bundler.io/). There
are only two Rubists in the world who don't use Bundler. I'm only half-kidding.
Bundler's main job is to help you deal with versions of the dependencies your
application needs. But since it does that, it also comes with an awesome
generator to help you make gems. It's going to do 90% of the work for us.

First, we need to install Bundler. It's as easy as

```bash
$ gem install bundler
```

Once that's done, we run a very similar, but different, command. This generates
a skeleton for our gem's meat to build on top of.

That sentence was kinda gross. Sorry. Anyway:

```bash
$ bundle gem how_i_start
      create  how_i_start/Gemfile
      create  how_i_start/Rakefile
      create  how_i_start/LICENSE.txt
      create  how_i_start/README.md
      create  how_i_start/.gitignore
      create  how_i_start/how_i_start.gemspec
      create  how_i_start/lib/how_i_start.rb
      create  how_i_start/lib/how_i_start/version.rb
Initializing git repo in /home/steve/src/how_i_start
$
```

See? Told you we needed `git`. Here's what these files do:

### Gemfile

The `Gemfile` is the main file that Bundler uses to track versions of all the
dependencies our gem needs. It does this through an interesting mechanism:

```
$ cat Gemfile
source 'https://rubygems.org'

# Specify your gem's dependencies in how_i_start.gemspec
gemspec
```

Surprise! Your dependencies are in another castle. Bundler knows how to figure
them out from our gemspec, which we'll talk about in a moment. We don't need
to edit this file at all, it works just fine.

### Rakefile

Rake is Ruby's version of the venerable Make tool for building things. We
don't need to edit this either, as it already contains the stuff needed to
do a bunch of cool things:

```bash
$ bundle exec rake -T
rake build    # Build how_i_start-0.0.1.gem into the pkg directory
rake install  # Build and install how_i_start-0.0.1.gem into system gems
rake release  # Create tag v0.0.1 and build and push how_i_start-0.0.1.gem to Ru...
```

These three commands help us with making a gem. `rake build` will attempt to
package up our gem. `rake install` will `rake build`, and then install it into
our Ruby, so we can give it a whirl. Finally, `rake release` will actually
release our gem. We'll talk more about all this later.

### LICENSE.txt

Rubyists almost exclusively love the MIT license, because it makes making money
really, really easy. I say all kinds of controversial political things on
Twitter, and everybody shrugs. As soon as I suggest that I *might* use the GPL,
people lose their cool.

You should use whatever license you want. I won't judge you. Everyone else might,
though. :/.

### README.md

Bundler gives us a pretty okay README to start with. We'll modify this more
soon. The `md` stands for Markdown, the One True Document Format.

### .gitignore

Bundler is kind enough to make sure to create a decent ignore file for `git`
so that we don't check bad things in. There's one thing that's different
when you're making a gem, and that's the `Gemfile.lock`. Normally, if you were
building an app, you'd check this in, but when you're making a gem, you don't.
If you don't know why this is, [go read
this](http://yehudakatz.com/2010/12/16/clarifying-the-roles-of-the-gemspec-and-gemfile/).

### how_i_start.gemspec

This file specifies all the metadata for our gem. The default values are
decent, but I'm going to edit them, and **then** show you the output.

Okay, I'm done. Here it is. Substitute your own details, unless you're me.

```ruby
$ cat how_i_start.gemspec
# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'how_i_start/version'

Gem::Specification.new do |spec|
  spec.name          = "how_i_start"
  spec.version       = HowIStart::VERSION
  spec.authors       = ["Steve Klabnik"]
  spec.email         = ["steve@steveklabnik.com"]
  spec.summary       = %q{A simple gem, to show you how I do things.}
  spec.description   = %q{A simple gem, to show you how I do things. If it were more complicated, I'd explain more about it here.}
  spec.homepage      = "https://steveklabnik.github.io/how_i_do_things"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.6"
  spec.add_development_dependency "rake"
end
```

The first part sets up loading paths. The real meat is in the block. It's all
pretty basic stuff. The second of the three blocks is kinda interesting, but
you never need to touch the generated files. Basically, they use Git Magic (tm)
to figure out which files should be included in your gem. This means that
you'll never accidentally distribute the wrong files, unless you forget to
commit them. And if they're not committed, they don't exist. You know it's
true.

The `require_paths` line is a Ruby convention: all the files for your library
go in `lib`.

The last two are interesting, too: they say that in order to work on our gem,
we need `bundler` and `rake`. If we were using a special test framework, we'd
add it here. `add_development_dependency` has a sibling method that we won't
use, but I feel like I should tell you about: `add_dependency`. If our gems
needed other gems to work, we'd use that to add them, here. These two methods
are what Bundler uses to figure out what to install when we're working on our
gem, and what Rubygems uses to figure out what to install when we're installing
our gem. TL;DR: they're super important.

Whew! That file is the most important, as it tells us where everything else
goes. Let's move on.

### lib/how_i_start.rb

As I mentioned, this file is in `lib` because that's where our files go. This
file has the same name as the gem, so it's the file that gets required when
you say `require "how_i_start"` in a Ruby program. Very important.

### lib/how_i_start/version.rb

This file contains just a few lines:

```ruby
$ cat lib/how_i_start/version.rb
module HowIStart
  VERSION = "0.0.1"
end
```

By keeping this constant in its own file, we can not load up our entire gem
when we need to check what version it is. Careful readers will notice that our
Gemspec did this, exactly.

You'll edit this file when you're about to release a new version of the gem.

## Write some tests

Step four! Every Rubyist except for DHH believes in test-driven development,
so we'll write a test first. This test will be very, very simple.

I prefer to use `minitest` for testing, as it's included with Ruby. I don't
find the extra complexity of other testing frameworks particularly worthwhile,
though I will say that RSpec's mocking framework is kinda nice.

To do this, we need to do a few things:

```bash
$ mkdir test
$ touch test/url_test.rb
```

This is the `minitest` convention. Test files are placed in `test` folder, ending with `_test.rb`.
We'll also need to add a few lines to the `Rakefile`:

```ruby
$ cat Rakefile
require "bundler/gem_tasks"

require "rake/testtask"

Rake::TestTask.new do |t|
  t.test_files = FileList['test/*_test.rb']
end

task default: :test
```

That first line was there from Bundler. It's what made the previous three tasks
we discussed. You have to add the rest. The first line requires the necessary
stuff from Rake. The second instantiates the task. The line in the block tells
the task where to find our files.  Finally, we set our default task to run our
tests.

Check to see if it works:

```bash
$ bundle exec rake
$ echo $?
0
$
```

Cool. We don't have any tests, so we don't have any output. Let's make a test!

```ruby
$ cat test/url_test.rb
require "minitest/autorun"

require "how_i_start"

class UrlTest < Minitest::Test
  def test_url
    assert_equal "http://howistart.org/posts/ruby/1", HowIStart::Url
  end
end
```

Easy enough! We start off by requiring the test runner, requiring our library,
and then we make a class to hold our test. One method, starting with `test_`, is
our test itself.  We have one simple assertion, which checks that we've set a
constant to the URL of this post. Nice and easy.

The most important part of TDD is to run your tests and watch them fail.
Here we go!

```bash
$ bundle exec rake
Run options: --seed 28249

# Running:

E

Finished in 0.001131s, 884.3719 runs/s, 0.0000 assertions/s.

  1) Error:
UrlTest#test_url:
NameError: uninitialized constant HowIStart::Url
    /home/steve/src/how_i_start/test/url_test.rb:5:in `test_url'

1 runs, 0 assertions, 0 failures, 1 errors, 0 skips
rake aborted!
Command failed with status (1): [ruby -I"lib" -I"/home/steve/.gem/ruby/2.1.1/gems/rake-10.3.2/lib" "/home/steve/.gem/ruby/2.1.1/gems/rake-10.3.2/lib/rake/rake_test_loader.rb" "test/url_test.rb" ]

Tasks: TOP => default => test
(See full trace by running task with --trace)
```

It doesn't know what our constant is. Perfect.

## Implement the code

Let's actually define our constant:

```ruby
$ cat lib/how_i_start.rb
require "how_i_start/version"

module HowIStart
  Url = "http://howistart.org/posts/ruby/1"
end
```

Note that that's indented by two spaces, no tabs. Frankly, indenting your code
with two spaces and no tabs is more important than getting the rest of the
syntax right, if you ask a random Rubyist.

Now we can run our test again:

```bash
$ bundle exec rake
Run options: --seed 43182

# Running:

.

Finished in 0.001069s, 935.6953 runs/s, 935.6953 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

:heart_eyes:. We've passed the test! That was easy!

Just defining a constant isn't quite enough, though. Let's add
an executable. Executables are stored in `bin`:

```bash
$ mkdir bin
```

Put this in it:

```ruby
$ cat bin/how_i_start
#!/usr/bin/env ruby

require 'how_i_start'

puts HowIStart::Url
```

This is actually a script. We don't add an extension, but we do use a 'shebang'
line to tell the shell that this is a Ruby script. We then load our library,
and print our constant.

I don't tend to test 'binaries.' All the logic should go in the gem itself, the
binary is just a thin shim to shove the command-line options to the Real Code.

## Write some docs

Use Rdoc. If it doesn't work, use [YARD](http://yardoc.org/). I include this
comment here because docs are really, really important. We don't really have
much to document, though. We'll just add some comments:

```ruby
$ cat lib/how_i_start.rb
require "how_i_start/version"

# All code in the gem is namespaced under this module.
module HowIStart

  # The URL of the article about how I start.
  Url = "http://howistart.org/posts/ruby/1"
end
$ cat lib/how_i_start/version.rb
module HowIStart

  # The current version of HowIStart.
  VERSION = "0.0.1"
end
```

Rdoc will parse these comments to generate documentation. You can get more
complicated than this, but it gives you the general idea. Generating the
documentation is really easy:

```bash
$ rdoc lib
Parsing sources...
100% [ 2/ 2]  lib/how_i_start/version.rb

Generating Darkfish format into /home/steve/src/how_i_start/doc...

  Files:      2

  Classes:    0 (0 undocumented)
  Modules:    1 (0 undocumented)
  Constants:  2 (0 undocumented)
  Attributes: 0 (0 undocumented)
  Methods:    0 (0 undocumented)

  Total:      3 (0 undocumented)
  100.00% documented

  Elapsed: 0.0s
$ firefox doc/index.html
```

RDoc can tell us if we're missing any documentation. Awesome. I always
open up the HTML docs in my browser to see if they look okay.

## Push to GitHub

Rubyists assume you use GitHub. It was originally created by some Rubyists,
lots of early users were Rubyists. If you like a different code hosting
platform, sorry. :/. I can only think of one gem that I use that doesn't use
GitHub.

Make a new GitHub repository with the same name as your gem, and then use
`git` to push it up:

```bash
$ git add .
$ git commit -m "Initial commit."
$ git remote add origin git@github.com:steveklabnik/how_i_start.git
$ git push -u origin master
```

Refresh at will. Neat!

Oh no! It looks like we didn't write a good README. Let's fix that.
Here's my diff:

```bash
$ git diff --cached
diff --git a/README.md b/README.md
index 9c664a3..731d6f7 100644
--- a/README.md
+++ b/README.md
@@ -1,24 +1,22 @@
 # HowIStart

-TODO: Write a gem description
+HowIStart is a very simple example gem to show you how I begin a Ruby project.

 ## Installation

-Add this line to your application's Gemfile:
-
-    gem 'how_i_start'
-
-And then execute:
-
-    $ bundle
-
-Or install it yourself as:
+Install it yourself as:

     $ gem install how_i_start

 ## Usage

-TODO: Write usage instructions here
+Just run the executable:
+
+```
+$ how_i_start
+```
+
+And it will point you at the article.

 ## Contributing
```

Let's commit that too:

```bash
$ git add README.md
$ git commit -m "Fix up README."
$ git push origin master
```

Much better.

## Push to RubyGems

Let's release this sucker!

First thing to do is to make sure that packaging it all up works. Let's try:

```bash
$ bundle exec rake install
how_i_start 0.0.1 built to pkg/how_i_start-0.0.1.gem.
how_i_start (0.0.1) installed.
steve@computer:~/src/how_i_start$ how_i_start
http://howistart.org/posts/ruby/1
```

Great! It successfully built the package, and our 'binary' works. Since this is
a feature-complete version of the gem, we should bump the version to 1.0.

```bash
$ cat lib/how_i_start/version.rb
module HowIStart

  # The current version of HowIStart.
  VERSION = "1.0.0"
end
$ git add lib/how_i_start/version.rb
$ git commit -m "Bump version for 1.0 release"
[master 499c1c0] Bump version for 1.0 release
 1 file changed, 1 insertion(+), 1 deletion(-)
$ bundle exec rake release
how_i_start 1.0.0 built to pkg/how_i_start-1.0.0.gem.
Tagged v1.0.0.
Pushed git commits and tags.
Pushed how_i_start 1.0.0 to rubygems.org.
```

We just increase the version, commit it, and then run the Rake task that
Bundler gave us. Since this isn't my first time, it uses my saved credentials,
but it might ask you for yours.

## Tweet about it

If a gem gets released in a forest, and nobody is there to hear it, it
certainly... yeah okay, that didn't really work out. My point is, if you make a
gem, and nobody knows about it, then it's not very useful. Promotion is hard,
but there is an answer: Twitter. [Even people that hate Twitter post stuff to
Twitter](https://twitter.com/peterc/status/475292959337103360). It's just the
way of the Ruby world.

So do this:

<blockquote class="twitter-tweet" lang="en"><p>I&#39;ve just released &quot;How I Start&quot; 1.0! &#10;&#10;$ gem install how_i_start&#10;&#10;<a href="https://t.co/R5m6ODxOZB">https://t.co/R5m6ODxOZB</a></p>&mdash; Brooklyn.rs (@steveklabnik) <a href="https://twitter.com/steveklabnik/statuses/479671271832444928">June 19, 2014</a></blockquote>

It's worth following a bunch of Ruby people on Twitter. Figure out who makes
the gems you use, and follow them. Getting to know people is cool, but even if
you hate that, you can [find out about things before they really
happen](https://twitter.com/steveklabnik/status/479671346314883073).

Don't be shy, though. You've made something useful! We all want to use it!

## Conclusion

That's it! None of it is particularly difficult by itself, but there is a bunch
when you put it all together. Please take these ten simple steps and write some
great gems! I know you have it in you!
