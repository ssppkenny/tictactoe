// This line opens the Tea.App modules into the current scope for Program access functions and types
open Tea.App

// This opens the Elm-style virtual-dom functions and types into the current scope
open Tea.Html
open Belt
// Let's create a new type here to be our main message type that is passed around
type msg = Coords(int, int) // This will be our message to increment the counter

// the model for Counter is just an integer
type state = list<list<(string, int)>>
type model = {
  state: list<list<(string, int)>>,
  move: bool,
}

// This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values
let init = () => {
  state: list{
    list{("left_upper", 0), ("center_upper", 0), ("right_upper", 0)},
    list{("left_middle", 0), ("center_middle", 1), ("right_middle", 0)},
    list{("left_lower", 0), ("center_lower", 0), ("right_lower", 0)},
  },
  move: false,
}

let freePos = (s: state) => {
  let fp = []
  s->List.forEachWithIndex((i, x) => {
    x->List.forEachWithIndex((j, y) => {
      let (_, v) = y
      if v == 0 {
        Js.Array.push((i, j), fp)->ignore
      }
    })
  })
  fp
}

let nextMove = (m: model, fp: array<(int, int)>) => {
  let s = m.state
  let l = Array.length(fp)
  let mv = m.move
  let r = Js.Math.random_int(0, l)
  let opt = fp->Array.get(r)

  let retVal = opt->Option.map(x => {
    let (i, j) = x
    let ns = s->List.mapWithIndex((n, x) => {
      x->List.mapWithIndex(
        (m, y) => {
          let (a, v) = y
          if n == i && m == j {
            if mv {
              (a, 2)
            } else {
              (a, 1)
            }
          } else {
            (a, v)
          }
        },
      )
    })
    ns
  })

  Option.getWithDefault(retVal, s)
}

// This is the central message handler, it takes the model as the first argument
let update = (model: model, msg: msg): model =>
  switch msg {
  | Coords(i, j) =>
    let s = model.state->List.mapWithIndex((n, x) =>
      x->List.mapWithIndex((m, y) => {
        let (a, v) = y
        let p = if model.move {
          1
        } else {
          2
        }
        if n == i && m == j {
          (a, p)
        } else {
          (a, v)
        }
      })
    )
    let fp = freePos(s)
    let nm: state = nextMove({state: s, move: model.move}, fp)
    {state: nm, move: model.move}
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
        model.state->List.mapWithIndex((i, x) =>
          div(
            list{Attributes.class("row")},
            x->List.mapWithIndex((j, y) => {
              let (s, v) = y
              let t = if v == 0 {
                ""
              } else if v == 1 {
                "x"
              } else {
                "o"
              }
              div(
                list{Attributes.class(s), Events.onClick(Coords(i, j))},
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
