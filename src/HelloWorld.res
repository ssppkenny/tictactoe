// This line opens the Tea.App modules into the current scope for Program access functions and types
open Tea.App

// This opens the Elm-style virtual-dom functions and types into the current scope
open Tea.Html

// Let's create a new type here to be our main message type that is passed around
type msg = Coords(int, int) // This will be our message to increment the counter

// the model for Counter is just an integer
type model = array<array<int>>

// This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values
let init = () => [[0, 0, 0], [0, 0, 0], [0, 0, 0]]

// This is the central message handler, it takes the model as the first argument
let update = (model: model, msg: msg): model =>
  switch msg {
  | Coords(i, j) => {
      model[i][j] = 1
      model
    }
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
        list{
          div(
            list{Attributes.class("row")},
            list{
              div(list{Attributes.class("left_upper")}, list{}),
              div(list{Attributes.class("center_upper")}, list{}),
              div(list{Attributes.class("right_upper")}, list{}),
            },
          ),
          div(
            list{Attributes.class("row")},
            list{
              div(list{Attributes.class("left_middle")}, list{}),
              div(
                list{Attributes.class("center_middle")},
                list{div(list{Attributes.class("circle")}, list{text("x")})},
              ),
              div(list{Attributes.class("right_middle")}, list{}),
            },
          ),
          div(
            list{Attributes.class("row")},
            list{
              div(list{Attributes.class("left_lower")}, list{}),
              div(list{Attributes.class("center_lower")}, list{}),
              div(list{Attributes.class("right_lower")}, list{}),
            },
          ),
        },
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
