// This line opens the Tea.App modules into the current scope for Program access functions and types
open Tea.App

// This opens the Elm-style virtual-dom functions and types into the current scope
open Tea.Html
open Belt
// Let's create a new type here to be our main message type that is passed around
type msg = Coords(int, int) // This will be our message to increment the counter

// the model for Counter is just an integer
type model = list<list<(string, int)>>

// This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values
let init = () => list{
  list{("left_upper", 0), ("center_upper", 0), ("right_upper", 0)},
  list{("left_middle", 0), ("center_middle", 1), ("right_middle", 0)},
  list{("left_lower", 0), ("center_lower", 0), ("right_lower", 0)},
}

// This is the central message handler, it takes the model as the first argument
let update = (model: model, msg: msg): model =>
  switch msg {
  | Coords(i, j) =>
    let m = model->List.mapWithIndex((n, x) =>
      x->List.mapWithIndex((m, y) => {
        let (a, _) = y
        if n == i && m == j {
          (a, 1)
        } else {
          (a, 0)
        }
      })
    )
    m
  }

// This is just a helper function for the view, a simple function that returns a button based on some argument
let viewButton = (title: string, msg: msg) => button(list{Events.onClick(msg)}, list{text(title)})

// This is the main callback to generate the virtual-dom.
// This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing
let view = (model: model): Vdom.t<msg> =>
  div(
    list{Attributes.class("wrapper")},
    list{
      div(
        list{Attributes.class("field")},
        model->List.map(x =>
          div(
            list{Attributes.class("row")},
            x->List.map(y => {
              let (s, v) = y
              let t = if v == 0 {
                "o"
              } else {
                "x"
              }
              div(
                list{Attributes.class(s)},
                list{div(list{Attributes.class("circle")}, list{text(t)})},
              )
            }),
          )
        ),
      ),
    },
  )

// This is the main function, it can be named anything you want but `main` is
// traditional.  The Program returned here has a set of callbacks that can easily be
// called from Rescript or from javascript for running this main attached to an
// element, or even to pass a message into the event loop.  You can even expose the
// constructors to the messages to javascript via the above [@@bs.deriving
// {accessors}] attribute on the `msg` type or manually, that way even javascript can
// use it safely.
let main = beginnerProgram({
  model: init(),
  update,
  view,
})
