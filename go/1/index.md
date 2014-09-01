Go is a lovely little programming language
designed by [smart people you can trust](http://en.wikipedia.org/wiki/Go_(programming_language)#History)
and continuously improved by [a large and growing open-source community](https://groups.google.com/forum/#!forum/golang-nuts).

Go is meant to be simple, but sometimes the conventions can be a little hard to grasp.
I'd like to show you how I start all of my Go projects, and how to use Go's idioms.
Let's build a backend service for a web app.

1. [Setting up your environment](#setting-up-your-environment)
1. [A new project](#a-new-project)
1. [Making a web server](#making-a-web-server)
1. [Adding more routes](#adding-more-routes)
1. [Querying multiple APIs](#querying-multiple-apis)
1. [Make it concurrent](#make-it-concurrent)
1. [Simplicity](#simplicity)
1. [Further exercises](#further-exercises)

## Setting up your environment

The first step is, of course, to install Go.
You can use [the binary distribution for your operating system](http://golang.org/doc/install) from the official site.
If you use Homebrew on Mac, `brew install go` works well.
When you're done, this should work:

```
$ go version
go version go 1.3.1 darwin/amd64
```

Once installed, the only other thing to do is to set your GOPATH.
This is the root directory that will hold all of your Go code and built artifacts.
The Go tooling will create 3 subdirectories in your GOPATH: bin, pkg, and src.
Some people set it to something like `$HOME/go`, but I prefer plain `$HOME`.
Make sure it gets exported to your environment.
If you use bash, something like this should work:

```
$ echo 'export GOPATH=$HOME' >> $HOME/.profile
$ source $HOME/.profile
$ go env | grep GOPATH
GOPATH="/Users/peter"
```

There are a lot of editors and plugins available for Go.
I'm personally a huge fan of Sublime Text and the excellent [GoSublime](https://github.com/DisposaBoy/GoSublime) plugin.
But the language is straightforward enough, especially for a small project, that a plain text editor is more than sufficient.
I work with professional, full-time Go developers who still use vanilla vim, without even syntax highlighting.
You definitely don't need more than that to get started.
As always, simplicity is king.

## A new project

With a functioning environment, we'll make a new directory for the project.
The Go toolchain expects all source code to exist within $GOPATH/src, so we always work there.
The toolchain can also directly import and interact with projects hosted on sites like GitHub or Bitbucket,
 assuming they live in the right place.

For this example, create a new, empty repository on GitHub. I'll assume it's called "hello".
Then, make a home for it in your $GOPATH.

```
$ mkdir -p $GOPATH/src/github.com/your-username
$ cd $GOPATH/src/github.com/your-username
$ git clone git@github.com:your-username/hello
$ cd hello
```

Great.
Create `main.go`, which will be our absolute-minimum Go program.

```go
package main

func main() {
    println("hello!")
}
```

Invoke `go build` to compile everything in the current directory.
It'll produce a binary with the same name as the directory.

```
$ go build
$ ./hello
hello!
```

Easy!
Even after several years of writing Go, I still start all of my new projects like this.
An empty git repo, a `main.go`, and a little bit of typing.

Since we took care to follow the common conventions, your application is automatically `go get`-able.
If you commit and push this single file to GitHub, anyone with a working Go installation should be able to do this:

```
$ go get github.com/your-username/hello
$ $GOPATH/bin/hello
hello!
```

## Making a web server

Let's turn our hello, world into a web server.
Here's the full program.

```go
package main

import "net/http"

func main() {
    http.HandleFunc("/", hello)
    http.ListenAndServe(":8080", nil)
}

func hello(w http.ResponseWriter, r *http.Request) {
    w.Write([]byte("hello!"))
}
```

There's a little bit to unpack.
First, we need to import the [net/http](http://golang.org/pkg/net/http) package from the standard library.

```go
import "net/http"
```

Then, in the main function, we install a handler function at the root path of our webserver.
[http.HandleFunc](http://golang.org/pkg/net/http/#HandleFunc) operates on the default HTTP router, officially called a
[ServeMux](http://golang.org/pkg/net/http/#ServeMux).

```go
http.HandleFunc("/", hello)
```

The function hello is an [http.HandlerFunc](http://golang.org/pkg/net/http/#HandlerFunc),
which means it has a specific type signature, and can be passed as an argument to HandleFunc.
Every time a new request comes into the HTTP server matching the root path, the server will spawn a new goroutine executing the hello function.
And the hello function simply uses the [http.ResponseWriter](http://golang.org/pkg/net/http/#ResponseWriter) to write a response to the client.
Since http.ResponseWriter.Write takes the more general `[]byte`, or byte-slice, as a parameter, we do a simple type conversion of our string.

```go
func hello(w http.ResponseWriter, r *http.Request) {
    w.Write([]byte("hello!"))
}
```

Finally, we start the HTTP server on port 8080 and with the default ServeMux via [http.ListenAndServe](http://golang.org/pkg/net/http/#ListenAndServe).
That's a synchronous, or blocking, call, which will keep the program alive until interrupted.
Compile and run just as before.

```
$ go build
./hello
```

And in another terminal, or your browser, make an HTTP request.

```
$ curl http://localhost:8080
hello!
```

Easy!
No frameworks to install, no dependencies to download, no project skeletons to create.
Even the binary itself is native code, statically linked, with no runtime dependencies.
Plus, the standard library's HTTP server is production-grade, with defenses against common attacks.
It can serve requests directly from the live internet—no intermediary required.

## Adding more routes

We can do something more interesting than just say hello.
Let's take a city as input, call out to a weather API, and forward a response with the temperature.
The [OpenWeatherMap](http://openweathermap.org/)
 provides a [simple and free API](http://openweathermap.org/api)
 for [current forecast info](http://openweathermap.org/current),
 which we can [query by city](http://api.openweathermap.org/data/2.5/weather?q=Tokyo).
It returns responses like this (partially redacted):

```json
{
    "name": "Tokyo",
    "coord": {
        "lon": 139.69,
        "lat": 35.69
    },
    "weather": [
        {
            "id": 803,
            "main": "Clouds",
            "description": "broken clouds",
            "icon": "04n"
        }
    ],
    "main": {
        "temp": 296.69,
        "pressure": 1014,
        "humidity": 83,
        "temp_min": 295.37,
        "temp_max": 298.15
    }
}
```

Go is a statically-typed language, so we should create a structure that mirrors this response format.
We don't need to capture every piece of information, just the stuff we care about.
For now, let's just get the city name and temperature, which is (hilariously) returned in Kelvin.
We'll define a struct to represent the data we need returned by the weather API.

```go
type weatherData struct {
    Name string `json:"name"`
    Main struct {
        Kelvin float64 `json:"temp"`
    } `json:"main"`
}
```

The `type` keyword defines a new type, which we call `weatherData`, and declare as a struct.
Each field in the struct has a name (e.g. `Name`, `Main`),
 a type (`string`, another anonymous `struct`),
 and what's known as a tag.
Tags are like metadata, and allow us to use the [encoding/json](http://golang.org/pkg/encoding/json) package to
 directly unmarshal the API's response into our struct.
It's a bit more typing compared to dynamic languages like Python or Ruby,
 but it gets us the highly desirable property of type safety.
For more about JSON and Go, see
[this blog post](http://blog.golang.org/json-and-go), or
[this example code](https://gobyexample.com/json).

We've defined the structure, and now we need to define a way to populate it.
Let's write a function to do that.

```go
func query(city string) (weatherData, error) {
    resp, err := http.Get("http://api.openweathermap.org/data/2.5/weather?q=" + city)
    if err != nil {
        return weatherData{}, err
    }

    defer resp.Body.Close()

    var d weatherData

    if err := json.NewDecoder(resp.Body).Decode(&d); err != nil {
        return weatherData{}, err
    }

    return d, nil
}
```

The function takes a string representing the city, and returns a weatherData struct and an error.
This is the fundamental error-handling idiom in Go.
Functions encode behavior, and behaviors typically can fail.
For us, the GET request against OpenWeatherMap can fail for any number of reasons,
and the data returned might not be what we expect.
In either case, we return a non-nil error to the client,
who's expected to deal it in a way that makes sense in the calling context.

If the [http.Get](http://golang.org/pkg/net/http/#Get) succeeds,
we [defer](http://golang.org/doc/effective_go.html#defer) a call to close the response body,
which will execute when we leave the function scope (when we return from the query function)
and is an elegant form of resource management.
Meanwhile, we allocate a weatherData struct, and use a [json.Decoder](http://golang.org/pkg/encoding/json/#Decoder)
to unmarshal from the response body directly into our struct.

As an aside, the [json.NewDecoder](http://golang.org/pkg/encoding/json/#NewDecoder) leverages an elegant feature of Go, which are
[interfaces](http://go-book.appspot.com/interfaces.html).
The Decoder doesn't take a concrete HTTP response body; rather, it takes an [io.Reader](http://golang.org/pkg/io/#Reader) interface,
which the [http.Response.Body](http://golang.org/pkg/net/http/#Response) happens to satisfy.
The Decoder supplies a behavior (Decode) which works just by invoking methods on types that satisfy other behaviors (Read).
In Go, we tend to implement behavior in terms of functions operating on interfaces.
It gives us a clean separation of data and control planes, easy testability with mocks, and code that's a lot easier to reason about.

Finally, if the decode succeeds, we return the weatherData to the caller, with a nil error to indicate success.
Now let's wire that function up to a request handler.

```go
http.HandleFunc("/weather/", func(w http.ResponseWriter, r *http.Request) {
    city := strings.SplitN(r.URL.Path, "/", 3)[2]

    data, err := query(city)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json; charset=utf-8")
    json.NewEncoder(w).Encode(data)
})
```

Here, we're definining the handler inline, rather than as a separate function.
We use [strings.SplitN](http://golang.org/pkg/strings/#SplitN)
 to take everything in the path after `/weather/` and treat it as the city.
We make our query, and if there's an error, we report it to the client with the
 [http.Error](http://golang.org/pkg/net/http/#Error) helper function.
We need to return at that point, so the HTTP request is completed.
Otherwise, we tell our client that we're going to send them JSON data, and use
[json.NewEncoder](http://golang.org/pkg/encoding/json/#NewEncoder) to JSON-encode the weatherData directly.

The code so far is nice and procedural, and easy to understand.
No opportunity for misinterpretation, and no way to miss the common errors.
If we move the "hello, world" handler to `/hello`, and make the necessary imports, we have our complete program:

```go
package main

import (
    "encoding/json"
    "net/http"
    "strings"
)

func main() {
    http.HandleFunc("/hello", hello)

    http.HandleFunc("/weather/", func(w http.ResponseWriter, r *http.Request) {
        city := strings.SplitN(r.URL.Path, "/", 3)[2]

        data, err := query(city)
        if err != nil {
            http.Error(w, err.Error(), http.StatusInternalServerError)
            return
        }

        w.Header().Set("Content-Type", "application/json; charset=utf-8")
        json.NewEncoder(w).Encode(data)
    })

    http.ListenAndServe(":8080", nil)
}

func hello(w http.ResponseWriter, r *http.Request) {
    w.Write([]byte("hello!"))
}

func query(city string) (weatherData, error) {
    resp, err := http.Get("http://api.openweathermap.org/data/2.5/weather?q=" + city)
    if err != nil {
        return weatherData{}, err
    }

    defer resp.Body.Close()

    var d weatherData

    if err := json.NewDecoder(resp.Body).Decode(&d); err != nil {
        return weatherData{}, err
    }

    return d, nil
}

type weatherData struct {
    Name string `json:"name"`
    Main struct {
        Kelvin float64 `json:"temp"`
    } `json:"main"`
}
```

Build and run it, same as before.

```
$ go build
$ ./hello
```

```
$ curl http://localhost:8080/weather/tokyo
{"name":"Tokyo","main":{"temp":295.9}}
```

Commit and push!

## Querying multiple APIs

Maybe we can build a more accurate temperature for a city, by querying and averaging multiple weather APIs.
Unfortunately for us, most weather APIs require authentication.
So, get yourself an API key for [Weather Underground](http://www.wunderground.com/weather/api).

Since we want the same behavior from all of our weather APIs, it makes sense to encode that behavior as an interface.

```go
type weatherProvider interface {
    temperature(city string) (float64, error) // in Kelvin, naturally
}
```

Now, we can transform our old OpenWeatherMap query function into a type that satisfies the weatherProvider interface.
Since we don't need to store any state to make the HTTP GET, we'll just use an empty struct.
And we'll add a simple line of logging, so we can see what's happening.

```go
type openWeatherMap struct{}

func (w openWeatherMap) temperature(city string) (float64, error) {
    resp, err := http.Get("http://api.openweathermap.org/data/2.5/weather?q=" + city)
    if err != nil {
        return 0, err
    }

    defer resp.Body.Close()

    var d struct {
        Main struct {
            Kelvin float64 `json:"temp"`
        } `json:"main"`
    }

    if err := json.NewDecoder(resp.Body).Decode(&d); err != nil {
        return 0, err
    }

    log.Printf("openWeatherMap: %s: %.2f", city, d.Main.Kelvin)
    return d.Main.Kelvin, nil
}
```

Since we only want to extract the Kelvin temperature from the response, we can define the response struct inline.
Otherwise, it's pretty much the same as the query function, just defined as a method on an openWeatherMap struct.
That way, we can use an instance of openWeatherMap as a weatherProvider.

Let's do the same for the Weather Underground.
The only difference is we need to provide an API key.
We'll store the key in the struct, and use it in the method.
It will be a very similar function.

(Note that the Weather Underground doesn't disambiguate cities quite as nicely as OpenWeatherMap.
We're skipping some important logic to handle ambiguous city names for the purposes of the example.)

```go
type weatherUnderground struct {
    apiKey string
}

func (w weatherUnderground) temperature(city string) (float64, error) {
    resp, err := http.Get("http://api.wunderground.com/api/" + w.apiKey + "/conditions/q/" + city + ".json")
    if err != nil {
        return 0, err
    }

    defer resp.Body.Close()

    var d struct {
        Observation struct {
            Celsius float64 `json:"temp_c"`
        } `json:"current_observation"`
    }

    if err := json.NewDecoder(resp.Body).Decode(&d); err != nil {
        return 0, err
    }

    kelvin := d.Observation.Celsius + 273.15
    log.Printf("weatherUnderground: %s: %.2f", city, kelvin)
    return kelvin, nil
}
```

Now that we have a couple of weather providers, let's write a function to query them all, and return the average temperature.
For simplicity, if we encounter any errors, we'll just give up.

```go
func temperature(city string, providers ...weatherProvider) (float64, error) {
    sum := 0.0

    for _, provider := range providers {
        k, err := provider.temperature(city)
        if err != nil {
            return 0, err
        }

        sum += k
    }

    return sum / float64(len(providers)), nil
}
```

Notice that the function definition is very close to the weatherProvider temperature method.
If we collect the individual weatherProviders into a type, and define the temperature method on that type,
we can implement a meta-weatherProvider, comprised of other weatherProviders.

```go
type multiWeatherProvider []weatherProvider

func (w multiWeatherProvider) temperature(city string) (float64, error) {
    sum := 0.0

    for _, provider := range w {
        k, err := provider.temperature(city)
        if err != nil {
            return 0, err
        }

        sum += k
    }

    return sum / float64(len(w)), nil
}
```

Perfect.
We can pass a multiWeatherProvider anywhere that accepts a weatherProvider.

Now, we can wire that up to our HTTP server, very similar to before.

```go
func main() {
    mw := multiWeatherProvider{
        openWeatherMap{},
        weatherUnderground{apiKey: "your-key-here"},
    }

    http.HandleFunc("/weather/", func(w http.ResponseWriter, r *http.Request) {
        begin := time.Now()
        city := strings.SplitN(r.URL.Path, "/", 3)[2]

        temp, err := mw.temperature(city)
        if err != nil {
            http.Error(w, err.Error(), http.StatusInternalServerError)
            return
        }

        w.Header().Set("Content-Type", "application/json; charset=utf-8")
        json.NewEncoder(w).Encode(map[string]interface{}{
            "city": city,
            "temp": temp,
            "took": time.Since(begin).String(),
        })
    })

    http.ListenAndServe(":8080", nil)
}
```

Compile, run, and GET, just as before.
In addition to the JSON response, you'll see some output in your server logs.

```
$ ./hello
2015/01/01 13:14:15 openWeatherMap: tokyo: 295.46
2015/01/01 13:14:16 weatherUnderground: tokyo: 273.15
```

```
$ curl http://localhost/weather/tokyo
{"city":"tokyo","temp":284.30499999999995,"took":"821.665230ms"}
```

Commit and push!

## Make it concurrent

Right now we just query the APIs synchronously, one after the other.
But there's no reason we couldn't query them at the same time.
That should decrease our response times.

To do that, we leverage Go's concurrency primitives: goroutines and channels.
We'll spawn each API query in its own goroutine, which will run concurrently.
We'll collect the responses in a single channel, and perform the average calculation when everything is finished.

```go
func (w multiWeatherProvider) temperature(city string) (float64, error) {
    // Make a channel for temperatures, and a channel for errors.
    // Each provider will push a value into only one.
    temps := make(chan float64, len(w))
    errs := make(chan error, len(w))

    // For each provider, spawn a goroutine with an anonymous function.
    // That function will invoke the temperature method, and forward the response.
    for _, provider := range w {
        go func(p weatherProvider) {
            k, err := p.temperature(city)
            if err != nil {
                errs <- err
                return
            }
            temps <- k
        }(provider)
    }

    sum := 0.0

    // Collect a temperature or an error from each provider.
    for i := 0; i < len(w); i++ {
        select {
        case temp := <-temps:
            sum += temp
        case err := <-errs:
            return 0, err
        }
    }

    // Return the average, same as before.
    return sum / float64(len(w)), nil
}
```

Now, our requests take as long as the slowest individual weatherProvider.
And we only needed to change the behavior of the multiWeatherProvider,
which, notably, still satisfies the simple, synchronous weatherProvider interface.

Commit and push!

## Simplicity

We've gone from 'hello world' to a concurrent, REST-ish backend server in a handful of steps and using only the Go standard library.
Our code can be fetched and deployed on [nearly any server architecture](https://golang.org/doc/install#requirements).
The resulting binary is self-contained and fast.
And, most importantly, the code is straightforward to read and reason about.
It can easily be maintained and extended, as necessary.
I believe all of these properties are a function of Go's steady and philosophic devotion to simplicity.
As Rob "Commander" Pike puts it,
[less is exponentially more](http://commandcenter.blogspot.com/2012/06/less-is-exponentially-more.html).

## Further exercises

[Fork](https://github.com/peterbourgon/how-i-start-go) the final code on github.

Can you add another weatherProvider? (Hint: [forecast.io](https://developer.forecast.io/) is a good one.)

Can you implement a timeout in the multiWeatherProvider? (Hint: look at [time.After](http://golang.org/pkg/time/#After).)
